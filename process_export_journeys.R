# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(readxl)
library(patchwork)
library(sf)
library(tigris)
library(countrycode)
library(scales)
library(leaflet)
library(htmlwidgets)

# don't use scientific notation for numbers
options(scipen = 0)

#########################
# US LNG exports data from DOE

us_exports <- read_csv("data/us_exports.csv") %>%
  mutate(imo = as.character(imo),
         # add country code for destination country
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         # add time period
         year = case_when(departure_date <= "2018-03-31" ~ "2017-2018",
                          TRUE ~ "2023-2024"))

# IMO codes of these tankers and year in which they exported
us_export_tankers_year <- us_exports %>%
  select(imo,year) %>%
  unique() 

#########################
# daily AIS positions data from Marine Traffic

mt_positions <- read_csv("data/mt_positions.csv") %>%
  mutate(imo = as.character(imo))

glimpse(mt_positions)

# plot distribution of speeds for these AIS pings, to help with definition of maneuvering status later on
ggplot(mt_positions, aes(x = speed_x10/10)) + 
  geom_histogram(binwidth = 1, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(limits = c(-1,25)) +
  scale_y_continuous(labels = comma) +
  xlab("speed (knots)") +
  ylab("AIS transponder detections") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
  
#########################
# IGU data with tanker propulsion, capacity etc

sheet_names <- excel_sheets("data/igu.xlsx")
igu_tankers <- map_df(sheet_names, ~ read_excel("data/igu.xlsx", sheet = .x) %>%
                        mutate(year = .x) %>%
                        mutate(across(where(is.character), str_squish))) %>%
  mutate(propulsion = case_when(grepl("Steam",propulsion) ~ "Steam",
                                TRUE ~ propulsion),
         propulsion = case_when(grepl("SSD",propulsion) ~ "SSDR",
                                TRUE ~ propulsion),
         propulsion = case_when(grepl("XDF",propulsion) ~ "X-DF",
                                TRUE ~ propulsion),
         propulsion = case_when(grepl("MEGI|ME-GI|MEGA",propulsion) ~ "ME-GI",
                                TRUE ~ propulsion),
         propulsion = case_when(grepl("TFDE|DFDE",propulsion) ~ "TFDE/DFDE",
                                TRUE ~ propulsion),
         imo =  as.character(imo),
         year = case_when(str_detect("2019", year) ~ "2017-2018", # these are manual adds for some (then new) tankers missing in 2018 report
                          str_detect("2017|2018",year) ~ "2017-2018",
                          str_detect("2023|2024",year) ~ "2023-2024")) %>%
  # if any variation in capacity within two-year period from which we've drawn IGU data, calculate mean
  group_by(imo,year,delivered,propulsion,type) %>%
  summarize(capacity = mean(capacity, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()
rm(sheet_names)

#########################
# join IGU to US exports data so we have propulsion, capacity, and delivery date for each vessel, needed later for emissions calculations

# filter IGU data for those involved in US exports for our two time periods, retaining that year variable
igu_tankers_us_export <- semi_join(igu_tankers, us_export_tankers_year, by = c("imo", "year")) %>%
  unique()

# join exports to IGU data
us_exports <- left_join(us_exports,igu_tankers_us_export, by = c("imo","year")) 

# summarize vessels in our data by type and propulsion
us_exports %>%
  group_by(propulsion,type) %>%
  summarize(tankers = n_distinct(imo),
            exports = n_distinct(imo,departure_date))
# Groups:   propulsion [7]
# propulsion type         tankers exports
# <chr>      <chr>          <int>   <int>
# 1 ME-GI      Conventional      54     297
# 2 ME-GI      FSU                1       7
# 3 SSDR       Conventional       2       3
# 4 STaGE      Conventional       7      35
# 5 Steam      Conventional      32      91
# 6 TFDE/DFDE  Conventional      99     482
# 7 TFDE/DFDE  FSRU               4      13
# 8 X-DF       Conventional     107     584
# 9 NA         NA                 2      20

# remove the two SSDRs, for which we can't calculate emissions, and floating storage vessels, retaining only Conventional LNG tankers 
# this code also removes two vessels with NA for type, absent from IGU data. On inspection, these are bunkering vessels which should be excluded
us_exports <- us_exports %>%
  filter(propulsion != "SSDR" & type == "Conventional")

# add departure_id field to uniquely identify each export journey (which may include more than one destination country)
us_exports <- us_exports %>%
  mutate(departure_id = paste(departure_date, imo, sep = "_"))
glimpse(us_exports)

#########################
# data on LNG ports from GEM
ports <- st_read("data/GEM-GGIT-LNG-Terminals-2024-01/GEM-GGIT-LNG-Terminals-dataset-2024-01.shp", quiet = TRUE) %>%
  clean_names() %>%
  # filter for potentially active import terminals
  filter(facil_type == "Import" & grepl("Operating|Retired|Idle|Mothballed",status)) %>%
  # add country code
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

#########################
# isolate AIS positions data for each journey

# US lower 48 with buffer of 100km, to filter out journeys that don't start at US ports
lower_48 <- states() %>%
  filter(STUSPS %in% state.abb & !grepl("HI|AK",STUSPS)) %>%
  st_transform("EPSG:4326")
lower_48_buffer <- lower_48 %>%
  st_buffer(dist = 100000) %>%
  summarize() %>%
  st_make_valid()
# quick map to examine the buffer
ggplot() + geom_sf(data = lower_48_buffer) +  geom_sf(data = lower_48) 
rm(lower_48)

# list to hold processed data
us_exports_positions <- list()

# loop to process data
for (d in unique(us_exports$departure_id)) {
  
  print(d)
  
  # select one journey
  us_export <- us_exports %>%
    filter(departure_id == d)
  
  # identify next departure for same vessel in the export data, if present
  next_departure <- us_exports %>% 
    filter(imo == us_export$imo[1] & departure_date > us_export$departure_date[1]) %>%
    filter(departure_date == min(departure_date))
  
  # create sf object for ports in the destination country/countries
  try(ports_destinations <- ports %>%
        filter(iso3c %in% us_export$iso3c & ((status == "Operating" & start_yr1 <= year(us_export$departure_date[1])) | (grepl("Retired|Idle|Mothballed",status) & start_yr1 <= year(us_export$departure_date[1]) & (stop_yr >= year(us_export$departure_date[1]) | is.na(stop_yr))))) %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326"))
  
  # filter the AIS positions data for the vessel and year in question
  try(potential_us_export_positions <- mt_positions %>%
        filter(year == us_export$year[1], imo == us_export$imo[1] & timestamp >= us_export$departure_date[1]) %>%
        mutate(departure_id = d))
  
  # if there is a next departure, retain only AIS positions data before that time
  if (nrow(next_departure) > 0) {
    try(potential_us_export_positions <- potential_us_export_positions %>%
          filter(timestamp < next_departure$departure_date[1]))  
  }
  
  # create an sf object for the filtered AIS positions data
  try(potential_us_export_positions <- potential_us_export_positions %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326"))
  
  # move to the next journey if first AIS ping is not within 100km of US coast or if we have no tracking data
  if (st_within(slice(potential_us_export_positions,1), lower_48_buffer, sparse = FALSE) == FALSE) next
  if (nrow(potential_us_export_positions) == 0) next
  
  # identify the index of the nearest possible destination port for each point
  try(nearest_indices <- st_nearest_feature(potential_us_export_positions, ports_destinations))
  
  # extract the nearest features based on the indices
  try(nearest_ports <- ports_destinations[nearest_indices, ] %>%
        select(term_name, country, iso3c))
  
  # calculate the distances in km from each point to the nearest potential destination port
  try(distances <- as.integer(st_distance(potential_us_export_positions, nearest_ports, by_element = TRUE)/1000))
  
  # add the distances as a new column to the points object
  try(potential_us_export_positions <- potential_us_export_positions %>%
        bind_cols(nearest_ports %>% st_drop_geometry()) %>%
        mutate(distance = distances)) 
  
  # define arrival events
  # arrivals are earliest point where speed < 5 knots and distance less than 20km for each destination country, then take latest if multiple destinations
  # these values chosen after examining maps for ~ 75 sample journeys
  try(arrival <- potential_us_export_positions %>%
        group_by(iso3c) %>%
        filter(distance < 20 & speed_x10 < 50) %>%
        summarize(timestamp = min(timestamp)) %>%
        ungroup() %>%
        summarize(timestamp = max(timestamp)))
  
  # move to the next journey if can't identify an arrival event
  if (arrival$timestamp == -Inf) next
  
  # filter journey and apply status (docked, underway, maneuvering)
  # using two cut-offs for maneuvering speed, below 5 knots or below 10 knots
  try(us_export_positions <- potential_us_export_positions %>%
        filter(timestamp <= arrival$timestamp) %>%
        mutate(status_5kt = case_when(distance < 20 & speed_x10 == 0 ~ "docked",
                                      speed_x10 < 50 ~ "maneuvering",
                                      distance < 20 & speed_x10 == 0 ~ "docked",
                                      TRUE ~ "underway"),
               status_10kt = case_when(distance < 20 & speed_x10 == 0 ~ "docked",
                                       speed_x10 < 100 ~ "maneuvering",
                                       TRUE ~ "underway")
      ))
  
 
  # make and save a leaflet map for inspection
  try(map <- leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = us_export_positions, 
                         radius = 0.2, 
                         label = paste(as.Date(us_export_positions$timestamp),
                                       "speed: ",
                                       us_export_positions$speed_x10,
                                       "nearest port: ",
                                       us_export_positions$term_name,
                                       "distance to port: ",
                                       us_export_positions$distance,
                                       "status: ",
                                       us_export_positions$status_5kt)
        )
  )
  
  try(saveWidget(map,
                 paste0("leaflet_maps/",d,".html"),
                 selfcontained = FALSE,
                 libdir = "js"))
  
  # charts to help diagnose any issues with processing of journeys
  try(plot1 <- ggplot(us_export_positions, aes(x = timestamp, y = speed_x10)) +
        geom_line()+
        geom_point())
  try(plot2 <- ggplot(us_export_positions, aes(x = timestamp, y = distance)) +
        geom_line(color = "red") +
        geom_point(color = "red"))
  try(plot <- plot1 / plot2)
  try(ggsave(paste0("plots/speeds_distances_charts/",d,".png"), plot, width = 20, height = 20, units = "cm"))
  
  
  # add export journey to list
  us_exports_positions[[length(us_exports_positions)+1]] <- us_export_positions
  
  # clean up environment
  rm(us_export,next_departure,potential_us_export_positions,us_export_positions,map,plot1,plot2,plot,distances,ports_destinations,nearest_indices,nearest_ports,arrival)
  gc() # reclaim memory
}
# clean up environment
rm(d,us_export,next_departure,potential_us_export_positions,us_export_positions,map,plot1,plot2,plot,distances,ports_destinations,nearest_indices,nearest_ports,arrival)
   

# how many export journeys have we captured?
n_distinct(us_exports$departure_id)
# [1] 1489
length(us_exports_positions)
# [1] 1336
1336/1489
# [1] 0.8972465
# this workflow captured positions data for 1336 out of 1489, or almost 90 per cent, of the journeys in the export data

#########################
# data to be removed, after examination of outliers and other anomalies revealed in maps and plots

# 2017-07-31_9401295 terminate after 2017-08-26
# 2017-11-05_9687021 terminate after 2017-12-04
# 2017-09-11_9761853 terminate after 2017-10-24
# 2017-11-15_9434266 lose tracking data before docking, remove
# 2023-12-21_9373010 miss docking, remove
# 2017-07-24_9654878 miss docking, remove
# 2017-07-13_9373008 miss docking, remove
# 2024-01-21_9869306 miss docking, remove

# create a single sf object from the data
us_exports_positions_sf <- bind_rows(us_exports_positions) %>%
  unique() # remove any duplicate rows

# removal of the erroneous/anomalous data
us_exports_positions_remove <- us_exports_positions_sf %>%
  filter(grepl("2017-11-15_9434266|2023-12-21_9373010|2017-07-24_9654878|2017-07-13_9373008|2024-01-21_9869306",departure_id)
         | (departure_id == "2017-09-11_9761853" & timestamp > "2017-10-24")
         | (departure_id == "2017-07-31_9401295" & timestamp > "2017-08-26")
         | (departure_id == "2017-11-05_9687021" & timestamp > "2017-12-04")) %>%
  st_drop_geometry()

us_exports_positions_sf <- anti_join(us_exports_positions_sf,us_exports_positions_remove)

n_distinct(us_exports_positions_sf$departure_id)
# [1] 1331
# we now have 1331 export journeys in the data

# ensure the statuses of the first and last AIS ping for each journey are docked
us_exports_positions_sf <- us_exports_positions_sf %>%
group_by(departure_id) %>%
  mutate(
    status_5kt = case_when(
      row_number() == 1 ~ "docked",                      
      row_number() == n() ~ "docked",                    
      TRUE ~ status_5kt),
    status_10kt = case_when(
        row_number() == 1 ~ "docked",                       
        row_number() == n() ~ "docked",
      TRUE ~ status_10kt)) %>%
  ungroup()

#########################
# what difference did the cut-off speed for maneuvering make?
us_exports_positions_sf %>% filter(status_5kt != status_10kt)
# Simple feature collection with 762 features and 13 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -167.7494 ymin: -39.4255 xmax: 166.9746 ymax: 57.80412
# Geodetic CRS:  WGS 84
# # A tibble: 762 × 14
# imo    timestamp           speed_x10 course heading year  departure_id             geometry term_name      country
# * <chr>  <dttm>                  <dbl>  <dbl>   <dbl> <chr> <chr>                 <POINT [°]> <chr>          <chr>  
#   1 93839… 2017-04-15 00:03:23        68    143     140 2017… 2017-04-08_…  (-79.52686 8.89425) Altamira LNG … Mexico 
# 2 94775… 2017-04-13 03:36:22        92    162     160 2017… 2017-04-10_…  (-86.09369 22.7377) Mejillones LN… Chile  
# 3 94775… 2017-04-14 02:44:08        94    130     129 2017… 2017-04-10_… (-83.56596 20.05055) Mejillones LN… Chile  
# 4 94775… 2017-04-15 00:00:02        87    130     129 2017… 2017-04-10_… (-80.93488 18.06141) Mejillones LN… Chile  
# 5 94775… 2017-04-16 00:23:12        94    191     188 2017… 2017-04-10_… (-78.25024 15.73058) Mejillones LN… Chile  
# 6 94775… 2017-04-17 00:45:21        95    192     190 2017… 2017-04-10_… (-79.25186 12.01312) Mejillones LN… Chile  
# 7 96816… 2017-04-20 00:01:50        72    142     142 2017… 2017-04-14_… (-79.52122 8.887883) Kushiro LNG T… Japan  
# 8 96816… 2017-05-09 00:01:48        87     81      86 2017… 2017-04-14_…  (135.3162 34.55581) Senboku 2 LNG… Japan  
# 9 96367… 2017-05-07 00:02:29        65    309     335 2017… 2017-04-16_… (-61.54815 -39.3189) Bahia Blanca … Argent…
# 10 96597… 2017-04-25 00:02:06        61    142     142 2017… 2017-04-17_…  (-79.56464 8.94325) Kushiro LNG T… Japan  
# # ℹ 752 more rows

# 752 out of 32868 rows in the data are classified differently

#########################
# clean up environment and save data for subsequent emissions calculations
rm(igu_tankers,igu_tankers_us_export,lower_48_buffer,mt_positions,ports,us_export_tankers_year,us_exports_positions,us_exports_positions_remove)

save.image("processed_data/us_export_journeys.RData")

