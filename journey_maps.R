# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(geosphere)
library(gdistance)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# load processed data
load("processed_data/us_exports_total_emissions.RData")

# don't use scientific notation for numbers
options(scipen = 0)

# world map
world <- ne_download(scale = 110, type = "countries", category = "cultural", destdir = tempdir())
st_crs(world)

world_extent <- extent(world)

# create a raster for cost calculation (assuming a global raster extent)
ras <- raster::raster(nrows = 1200, ncols = 2400, ext = world_extent)
crs(ras) <- CRS("EPSG:4326")
extent(ras)

# rasterize with a mask for land
worldmask <- raster::rasterize(world, ras)
worldras <- is.na(worldmask)

# set land cells to 999, representing a high cost, water cells are left with value of 1
worldras[worldras == 0] <- 999
plot(worldras)

# load the Panama and Suez canals data, buffered to a degree
canals <- st_read("data/canals_buffered.geojson")

# rasterize, setting values to 1 where there is an overlap (or any non-NA value)
canalsras <- rasterize(canals, ras, field = 1, background = NA)

# Assign a value of 1 to worldras where canals_ras is not NA (indicating an overlap)
worldras[!is.na(canalsras)] <- 1
# this should ensure that canal zones are passable when we calculate shortest paths

# transition layer matrix to calculate shortest paths
system.time({
  tr <- transition(worldras, function(x) {
    # check for invalid values (e.g., 999) and handle them
    mean_value <- mean(x, na.rm = TRUE)
    # if the mean is equal to 999, we can't calculate a valid transition (impassable area)
    if (mean_value == 999) {
      return(999)  # No transition in impassable areas, set to high cost
    }
    # for valid areas, calculate transition cost (1 / mean value)
    if (mean_value == 0) {
      return(1)  # neutral cost if mean is zero (unlikely in this case)
    }
    return(1 / mean_value)  # Normal cost calculation
  }, directions = 16)
  # Apply geoCorrection to the transition matrix
  tr <- geoCorrection(tr, "c")
})
# user  system elapsed 
# 378.678  23.065 405.820 

summary(tr@transitionMatrix)

# journeys of interest for maps

# CHL long 2024-02-18_9336749 via S America
# CHL short 2017-10-18_9434266 via Panama
# 
# TWN long 2023-05-04_9854612 via S Africa
# TWN short 2018-02-22_9624926 via Panama
# 
# CHN long 2024-02-12_9946374 via S Africa
# CHN short 2018-01-29_9721736 via Panama
# 
# JPN long 2023-05-01_9796781 via S Africa
# JPN short 2017-04-17_9659725 via Panama

# went into Med but then went round S Africa to Singapore
# 2023-10-26_9626273

journeys_of_interest <- c("2024-02-18_9336749","2017-10-18_9434266","2023-05-04_9854612","2018-02-22_9624926",
                          "2024-02-12_9946374","2018-01-29_9721736","2023-05-01_9796781","2017-04-17_9659725",
                          "2023-10-26_9626273")

# calculating geodesic paths
for (d in journeys_of_interest) {
  outward <- us_exports_positions_sf %>%
    filter(departure_id == d)
  coords <- st_coordinates(outward)
  linestring_geodesic <- NULL
  tryCatch({
    for (i in 1:(nrow(coords) - 1)) {
      segment <- gcIntermediate(coords[i, ], coords[i + 1, ], addStartEnd = TRUE, sp = TRUE)
      linestring_geodesic <- if (is.null(linestring_geodesic)) {
        st_as_sf(segment)
      } else {
        rbind(linestring_geodesic, st_as_sf(segment))
      }
    }
  }, error = function(e) {
    # Log error message and value of d
    cat("An error occurred for 5kt cut-off at iteration i =", i, "for departure_id =", d, ": ", e$message, "\n")
  })
  linestring_geodesic <- st_wrap_dateline(linestring_geodesic) %>%
    st_transform("EPSG:4326")
  st_write(linestring_geodesic, paste0("processed_data/",d,"_geodesic.geojson"))
  }
rm(segment,linestring_geodesic,i,d,coords,outward)

# calculating path avoiding land using the transition matrix
for (d in journeys_of_interest) {
  print(d)
  outward <- us_exports_positions_sf %>%
    filter(departure_id == d)
  outward_sp <- as(outward, "Spatial")
  # calculate pairwise shortest paths and store them in a list
  path <- list()
  for (i in 1:(length(outward_sp) - 1)) {
    path[[i]] <- shortestPath(tr,
                              outward_sp[i,],
                              outward_sp[i + 1,],
                              output = "SpatialLines")
  }
  path_sf <- lapply(path, st_as_sf)
  path_sf <- bind_rows(path_sf) %>%
    st_set_crs("EPSG:4326")
  gc()
  st_write(path_sf, paste0("processed_data/",d,"_avoiding_land.geojson"))
}
rm(segment,linestring_geodesic,i,d,coords,outward,outward_sp,path,path_sf)

# then manual editing in QGIS combining elements of the two paths for best track 
# (geodesic but substituting segments from transition matrix path when geodesic crosses land )