# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(gdistance)
library(tidyverse)
library(sf)
library(geosphere)
library(scales)

# don't use scientific notation for numbers
options(scipen = 0)

#########################
# load processed export journeys data

load("processed_data/us_exports_journeys.RData")

##########################
# summarize data for each journey, calculating distance and days maneuvering metrics needed for emissions calculations

# data frame to hold the summary data
us_exports_summary <- tibble()

# loop to summarize data
n <- 1

for (d in unique(us_exports_positions_sf$departure_id)) {
  
  print(n)
  
  # filter data for one journey
  journey = us_exports_positions_sf %>%
    filter(departure_id == d)
  
  # count days maneuvering on outward journey for two cut-off maneuvering speeds (5 and 10 knots)
  days_maneuvering_out_5kt = nrow(
    journey %>%
      st_drop_geometry() %>%
      filter(status_5kt == "maneuvering") %>%
      unique()
  )
  
  days_maneuvering_out_10kt = nrow(
    journey %>%
      st_drop_geometry() %>%
      filter(status_10kt == "maneuvering") %>%
      unique()
  )
  
  # count days docked on outward journey 
  days_docked_out_5kt = nrow(
    journey %>%
      st_drop_geometry() %>%
      filter(status_5kt == "docked") %>%
      unique()
  )
  
  # count days docked on outward journey 
  days_docked_out_10kt = nrow(
    journey %>%
      st_drop_geometry() %>%
      filter(status_10kt == "docked") %>%
      unique()
  ) 
  
  # filter out the maneuvering data for each cut-off speed and extract coordinates
  not_maneuvering_5kt <- journey %>%
    filter(status_5kt != "maneuvering") %>%
    arrange(timestamp)
  
  not_maneuvering_10kt <- journey %>%
    filter(status_10kt != "maneuvering") %>%
    arrange(timestamp)
  
  coords_5kt <- st_coordinates(not_maneuvering_5kt)
  
  coords_10kt <- st_coordinates(not_maneuvering_10kt)
  
  # create geodesic paths between consecutive non-maneuvering points, for each cut-off speed

  linestring_geodesic_5kt <- NULL
  tryCatch({
    for (i in 1:(nrow(coords_5kt) - 1)) {
      segment_5kt <- gcIntermediate(coords_5kt[i, ], coords_5kt[i + 1, ], addStartEnd = TRUE, sp = TRUE)
      linestring_geodesic_5kt <- if (is.null(linestring_geodesic_5kt)) {
        st_as_sf(segment_5kt)
      } else {
        rbind(linestring_geodesic_5kt, st_as_sf(segment_5kt))
      }
    }
  }, error = function(e) {
    # Log error message and value of d
    cat("An error occurred for 5kt cut-off at iteration i =", i, "for departure_id =", d, ": ", e$message, "\n")
  })
  simple_path_5kt <- st_union(linestring_geodesic_5kt)  
  simple_path_5kt <- st_wrap_dateline(simple_path_5kt)
  simple_path_distance_out_km_5kt <- as.double(st_length(simple_path_5kt)/1000)
  
  linestring_geodesic_10kt <- NULL
  tryCatch({
    for (i in 1:(nrow(coords_10kt) - 1)) {
      segment_10kt <- gcIntermediate(coords_10kt[i, ], coords_10kt[i + 1, ], addStartEnd = TRUE, sp = TRUE)
      linestring_geodesic_10kt <- if (is.null(linestring_geodesic_10kt)) {
        st_as_sf(segment_10kt)
      } else {
        rbind(linestring_geodesic_10kt, st_as_sf(segment_10kt))
      }
    }
  }, error = function(e) {
    # Log the error message and value of d
    cat("An error occurred for 10kt cut-off at iteration i =", i, "for departure_id =", d, ": ", e$message, "\n")
  })
  simple_path_10kt <- st_union(linestring_geodesic_10kt)  
  simple_path_10kt <- st_wrap_dateline(simple_path_10kt)
  simple_path_distance_out_km_10kt <- as.double(st_length(simple_path_10kt)/1000)
  
  summary <- tibble(departure_id = d, 
                    days_maneuvering_out_5kt = days_maneuvering_out_5kt,
                    days_docked_out_5kt = days_docked_out_5kt,
                    simple_path_distance_out_km_5kt = simple_path_distance_out_km_5kt,
                    days_maneuvering_out_10kt = days_maneuvering_out_10kt,
                    days_docked_out_10kt = days_docked_out_10kt,
                    simple_path_distance_out_km_10kt = simple_path_distance_out_km_10kt,
                    )
  
  us_exports_summary <- bind_rows(us_exports_summary,summary)
  
  rm(journey,not_maneuvering_5kt,not_maneuvering_10kt,coords_5kt,coords_10kt,days_maneuvering_out_5kt,days_maneuvering_out_10k,days_docked_out_5kt,days_docked_out_10kt,segment_5kt,segment_10kt,linestring_geodesic_5kt,linestring_geodesic_10kt,simple_path_5kt,simple_path_10kt,simple_path_distance_out_km_5kt,simple_path_distance_out_km_10kt,summary)
  n <- n+1 
  gc()
}

# remove data for paths that failed to process completely

# from the error logs
# An error occurred for 5kt cut-off at iteration i = 23 for departure_id = 2023-09-21_9627502 :  NA values in coordinates 
# An error occurred for 10kt cut-off at iteration i = 23 for departure_id = 2023-09-21_9627502 :  NA values in coordinates 
# An error occurred for 5kt cut-off at iteration i = 54 for departure_id = 2023-10-31_9627502 :  NA values in coordinates 
# An error occurred for 10kt cut-off at iteration i = 54 for departure_id = 2023-10-31_9627502 :  NA values in coordinates 
# An error occurred for 5kt cut-off at iteration i = 27 for departure_id = 2024-01-02_9627502 :  NA values in coordinates 
# An error occurred for 10kt cut-off at iteration i = 25 for departure_id = 2024-01-02_9627502 :  NA values in coordinates 
# An error occurred for 5kt cut-off at iteration i = 22 for departure_id = 2024-02-09_9627502 :  NA values in coordinates 
# An error occurred for 10kt cut-off at iteration i = 22 for departure_id = 2024-02-09_9627502 :  NA values in coordinates

us_exports_summary <- us_exports_summary %>%
  filter(!grepl("2024-02-09_9627502|2024-01-02_9627502|2023-10-31_9627502|2023-09-21_9627502", departure_id)) %>%
  mutate(imo = word(departure_id, sep = "_",2),
         year = case_when(grepl("2017|2018", substr(departure_id,1,4)) ~ "2017-2018",
                          TRUE ~ "2023-2024"))

1327/1489
# [1] 0.8912021
# we now have data for 1327/1489 or about 89% of export journeys

# clean up environment
rm(d,i,n)

##########################################################################
# create summary data with destination countries for each export journey

journey_destinations <- us_exports %>%
  group_by(departure_id) %>%
  summarize(
    destinations = paste(iso3c, collapse = ", ")
  ) %>%
  # remove duplicates and sort alphabetically
  mutate(destinations = sapply(strsplit(destinations, ",\\s*"), function(x) {
    paste(sort(unique(x)), collapse = ", ")
  })) %>%
  # create a simplified destinations2 variable for use in later emissions modelling for missing data 
  rowwise() %>%  
  mutate(destinations2 = {
    unique_values <- strsplit(destinations, ",\\s*")[[1]] %>% unique()
    case_when(
      length(unique_values) == 1 ~ unique_values[1],                # single country code
      length(unique_values) == 2 ~ "two",                           # two country codes
      length(unique_values) == 3 ~ "three",                         # three country codes
      # TRUE ~ "many"                                                 # more - not in this data
    )
  }) %>%
  ungroup() %>%
  # add number of destinations as numeric variable
  mutate(n_destinations = case_when(destinations2 == "three" ~ 3,
                                    destinations2 == "two" ~ 2,
                                    TRUE ~ 1))

##############################################
# data on tankers (propulsion, delivered year, capacity) for each journey
tankers <- us_exports %>%
  dplyr::select(departure_id,propulsion,delivered,capacity) %>%
  unique()

##############################################
# join summary data for each processed export journey
us_exports_summary <- inner_join(us_exports_summary,tankers, by = "departure_id") %>%
  inner_join(journey_destinations, by = "departure_id")

##############################################
# calculate emissions using methods from Rosselot et al (2023) paper and its supplementary information 
# for each maneuvering cut-off speed

# 5 knot cut-off for maneuvering speed
emissions_5kt <- us_exports_summary %>%
  mutate(
    distance_total = simple_path_distance_out_km_5kt * 2, # accounting for outward and return journey 
    days_docked = days_docked_out_5kt,
    days_maneuvering = days_maneuvering_out_5kt + (0.5 * n_destinations), 
    containment_factor = case_when(delivered < 2000 ~ 2,
                                   delivered >= 2017 ~ 1,
                                   TRUE ~ -0.06*(delivered-2000)+2),
    surface_area_factor = 10*pi*(capacity/4/pi)^(2/3)/18115.1859151843,
    propulsion_factor = case_when(propulsion == "Steam" ~ 1,
                                  propulsion != "Steam" & delivered < 1994 ~ 6.7,
                                  propulsion != "Steam" & delivered > 2020 ~ 1,
                                  TRUE ~ -0.21*(delivered-2000)+5.2),
    boil_off_underway_total = distance_total*containment_factor*surface_area_factor*0.0803101327514633,
    boil_off_underway_generators = case_when(grepl("STaGE|DFDE", propulsion) ~ 0,
                                             propulsion == "ME-GI" ~ distance_total*containment_factor*surface_area_factor*0.019636083856571*1.12,
                                             TRUE ~ distance_total*containment_factor*surface_area_factor*0.019636083856571),
    boil_off_underway_propulsion = boil_off_underway_total - boil_off_underway_generators,
    # boil_off_underway_gcu = 0,
    boil_off_maneuvering_total = days_maneuvering*containment_factor*surface_area_factor*42.6817647058824,
    boil_off_maneuvering_generators = days_maneuvering*containment_factor*surface_area_factor*16.4876470588235,
    # boil_off_maneuvering_propulsion = 0,
    boil_off_maneuvering_gcu = days_maneuvering*containment_factor*surface_area_factor*26.1941176470588,
    boil_off_docked_total = days_docked*containment_factor*surface_area_factor*24.9648484848485,
    boil_off_docked_generators = days_docked*containment_factor*surface_area_factor*19.6587878787879,
    # boil_off_docked_propulsion = 0,
    boil_off_docked_gcu = days_docked*containment_factor*surface_area_factor*5.30606060606061,
    boil_off_total = boil_off_underway_total + boil_off_maneuvering_total + boil_off_docked_total,
    methane_slip_underway_generators = boil_off_underway_generators*0.083404937269912,
    methane_slip_underway_propulsion = case_when(grepl("DFDE|X-DF", propulsion) ~ boil_off_underway_propulsion*0.0218575669591883*propulsion_factor,
                                                 propulsion == "STaGE" ~ boil_off_underway_propulsion*0.0218575669591883*propulsion_factor*0.5 + boil_off_underway_propulsion*0.00005*propulsion_factor*0.5,
                                                 propulsion == "Steam" ~  boil_off_underway_propulsion*0.00005*propulsion_factor,
                                                 propulsion == "ME-GI" ~  boil_off_underway_propulsion*0.002*propulsion_factor),
    # methane_slip_underway_gcu = 0,
    methane_slip_maneuvering_generators = boil_off_maneuvering_generators*0.0820578686360555,
    # methane_slip_maneuvering_propulsion = 0,
    # methane_slip_maneuvering_gcu = 0,		   
    methane_slip_docked_generators = boil_off_docked_generators*0.0878626260135031,
    # methane_slip_docked_propulsion = 0,
    # methane_slip_docked_gcu = 0
    ) %>%
  rowwise() %>%
  mutate(methane_emissions = sum(c_across(contains("methane")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(co2_emissions =  case_when(propulsion == "ME-GI" ~ 1.07 * (boil_off_total-methane_emissions/0.915767234575348)*1.04890591912877/16.729037729152*44.00995,
                                    TRUE ~  (boil_off_total-methane_emissions/0.915767234575348)*1.04890591912877/16.729037729152*44.00995),
         total_emissions_co2_equivalent = co2_emissions + (methane_emissions * 82.5)
  )

# plot distribution of total emissions
ggplot(emissions_5kt, aes(x=total_emissions_co2_equivalent)) + 
  geom_histogram(binwidth = 1000, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(labels = comma) +
  xlab(expression("Total emissions, metric tons CO"[2] * " equivalent")) +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# plot distribution of days maneuvering
ggplot(emissions_5kt, aes(x=days_maneuvering)) + 
  geom_histogram(binwidth = 1, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(limits = c(0,25)) +
  xlab("Days maneuvering") +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# plot distribution of distances
ggplot(emissions_5kt, aes(x=distance_total)) + 
  geom_histogram(binwidth = 2500, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(labels = comma) +
  xlab("Return journey distance (km)") +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# 10 knot cut-off for maneuvering speed
emissions_10kt <- us_exports_summary %>%
  mutate(
    distance_total = simple_path_distance_out_km_10kt * 2, # accounting for outward and return journey 
    days_docked = days_docked_out_10kt,
    days_maneuvering = days_maneuvering_out_10kt + (0.5 * n_destinations), 
    containment_factor = case_when(delivered < 2000 ~ 2,
                                   delivered >= 2017 ~ 1,
                                   TRUE ~ -0.06*(delivered-2000)+2),
    surface_area_factor = 10*pi*(capacity/4/pi)^(2/3)/18115.1859151843,
    propulsion_factor = case_when(propulsion == "Steam" ~ 1,
                                  propulsion != "Steam" & delivered < 1994 ~ 6.7,
                                  propulsion != "Steam" & delivered > 2020 ~ 1,
                                  TRUE ~ -0.21*(delivered-2000)+5.2),
    boil_off_underway_total = distance_total*containment_factor*surface_area_factor*0.0803101327514633,
    boil_off_underway_generators = case_when(grepl("STaGE|DFDE", propulsion) ~ 0,
                                             propulsion == "ME-GI" ~ distance_total*containment_factor*surface_area_factor*0.019636083856571*1.12,
                                             TRUE ~ distance_total*containment_factor*surface_area_factor*0.019636083856571),
    boil_off_underway_propulsion = boil_off_underway_total - boil_off_underway_generators,
    # boil_off_underway_gcu = 0,
    boil_off_maneuvering_total = days_maneuvering*containment_factor*surface_area_factor*42.6817647058824,
    boil_off_maneuvering_generators = days_maneuvering*containment_factor*surface_area_factor*16.4876470588235,
    # boil_off_maneuvering_propulsion = 0,
    boil_off_maneuvering_gcu = days_maneuvering*containment_factor*surface_area_factor*26.1941176470588,
    boil_off_docked_total = days_docked*containment_factor*surface_area_factor*24.9648484848485,
    boil_off_docked_generators = days_docked*containment_factor*surface_area_factor*19.6587878787879,
    # boil_off_docked_propulsion = 0,
    boil_off_docked_gcu = days_docked*containment_factor*surface_area_factor*5.30606060606061,
    boil_off_total = boil_off_underway_total + boil_off_maneuvering_total + boil_off_docked_total,
    methane_slip_underway_generators = boil_off_underway_generators*0.083404937269912,
    methane_slip_underway_propulsion = case_when(grepl("DFDE|X-DF", propulsion) ~ boil_off_underway_propulsion*0.0218575669591883*propulsion_factor,
                                                 propulsion == "STaGE" ~ boil_off_underway_propulsion*0.0218575669591883*propulsion_factor*0.5 + boil_off_underway_propulsion*0.00005*propulsion_factor*0.5,
                                                 propulsion == "Steam" ~  boil_off_underway_propulsion*0.00005*propulsion_factor,
                                                 propulsion == "ME-GI" ~  boil_off_underway_propulsion*0.002*propulsion_factor),
    # methane_slip_underway_gcu = 0,
    methane_slip_maneuvering_generators = boil_off_maneuvering_generators*0.0820578686360555,
    # methane_slip_maneuvering_propulsion = 0,
    # methane_slip_maneuvering_gcu = 0,		   
    methane_slip_docked_generators = boil_off_docked_generators*0.0878626260135031,
    # methane_slip_docked_propulsion = 0,
    # methane_slip_docked_gcu = 0
  ) %>%
  rowwise() %>%
  mutate(methane_emissions = sum(c_across(contains("methane")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(co2_emissions =  case_when(propulsion == "ME-GI" ~ 1.07 * (boil_off_total-methane_emissions/0.915767234575348)*1.04890591912877/16.729037729152*44.00995,
                                    TRUE ~  (boil_off_total-methane_emissions/0.915767234575348)*1.04890591912877/16.729037729152*44.00995),
         total_emissions_co2_equivalent = co2_emissions + (methane_emissions * 82.5)
  )

# plot distribution of total emissions
ggplot(emissions_10kt, aes(x=total_emissions_co2_equivalent)) + 
  geom_histogram(binwidth = 1000, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(labels = comma) +
  xlab(expression("Total emissions, metric tons CO"[2] * " equivalent")) +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# plot distribution of days maneuvering
ggplot(emissions_10kt, aes(x=days_maneuvering)) + 
  geom_histogram(binwidth = 1, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(limits = c(0,25)) +
  xlab("Days maneuvering") +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# plot distribution of days maneuvering
ggplot(emissions_10kt, aes(x=days_maneuvering)) + 
  geom_histogram(binwidth = 1, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(limits = c(0,25)) +
  xlab("Days maneuvering") +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# plot distribution of distances
ggplot(emissions_10kt, aes(x=distance_total)) + 
  geom_histogram(binwidth = 2500, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(labels = comma) +
  xlab("Return journey distance (km)") +
  ylab("Export journeys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

############################################## 
# calculate durations of each outward journey
durations <- us_exports_positions_sf %>%
  st_drop_geometry() %>%
  group_by(departure_id, year) %>%
  summarize(duration_out = round(max(timestamp) - min(timestamp)))

# plot distribution of durations by year
ggplot(durations, aes(x=duration_out)) + 
  geom_histogram(binwidth = 1, fill = "red") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(limits = c(0,100)) +
  xlab("Outward journey duration") +
  ylab("Export journeys") +
  facet_wrap(~year, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#########################
# total calculated emissions for each maneuvering cut-off speed, by year

emissions_5kt_year <- emissions_5kt %>%
  group_by(year) %>%
  summarize(
    export_journeys = n_distinct(departure_id),
    methane_emissions = sum(methane_emissions),
    co2_emissions = sum(co2_emissions),
    total_emissions_co2_equivalent = sum(total_emissions_co2_equivalent)
  )

emissions_10kt_year <- emissions_10kt %>%
  group_by(year) %>%
  summarize(
    export_journeys = n_distinct(departure_id),
    methane_emissions = sum(methane_emissions),
    co2_emissions = sum(co2_emissions),
    total_emissions_co2_equivalent = sum(total_emissions_co2_equivalent)
  )

##########################
# save processed data

save.image("processed_data/us_exports_emissions.RData")

###########################
# plot of correlation between methane and co2 emissions, faceted by year

ggplot(emissions_5kt, aes(x=methane_emissions, y=co2_emissions, color = propulsion)) + 
  geom_point() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  scale_y_continuous(labels = comma) +
  scale_color_discrete(name = "Propulsion") +
  facet_wrap(~year) +
  theme_minimal() +
  ylab(expression("CO"[2] * " emissions, metric tons")) +
  xlab("Methane emissions, metric tons") 

