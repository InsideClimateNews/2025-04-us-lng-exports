# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(glmnet)

# don't use scientific notation for numbers
options(scipen = 0)

#########################
# load processed export journeys data
load("processed_data/us_exports_emissions.RData")

# remove data from sensitivity analysis assuming 10 knot cut-off for maneuvering speed
rm(emissions_10kt,emissions_10kt_year)

#########################
# modeling of emissions by XGBoost regression 

# grid to tune hyperparameters for XGBoost models
set.seed(123)
grid <- grid_random(
  trees(range = c(500,1500)),
  tree_depth(range = c(3, 10)),
  min_n(range = c(2, 10)),  
  learn_rate(),
  size = 50
)

###########
# modeling of emissions calculated assuming 5 knot cut-off for maneuvering speed

# ensure variables are appropriately encoded
emissions_5kt <- emissions_5kt %>%
  mutate(
    destinations = as.factor(destinations),
    destinations2 = as.factor(destinations2),
    year = as.factor(year),
    propulsion = as.factor(propulsion),
    delivered = as.numeric(delivered) 
  )

# define gradient boosting model with hyperparameters to tune
gb_mod <- boost_tree(
  mode = "regression",                
  engine = "xgboost",                 
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(), 
  learn_rate = tune()
)


################################
# methane emissions

# define workflow to train model on calculated methane emissions
workflow_gb <- workflow() %>%
  add_model(gb_mod) %>%
  add_formula(methane_emissions ~ destinations2 + year + propulsion + capacity + delivered)

# define cross-validation splits
set.seed(123)
cv_splits <- vfold_cv(emissions_5kt, v = 5)

# tune the model using cross-validation
set.seed(123)
tune_results <- tune_grid(
  workflow_gb,              
  resamples = cv_splits, 
  grid = grid,
  metrics = metric_set(rmse, rsq, mae)  # evaluation metrics
)

# collect and assess the results
tune_metrics <- tune_results %>%
  collect_metrics()

sort_rsme <- tune_metrics %>%
  filter(.metric == "rmse") %>%
  arrange(mean)

sort_rsq <- tune_metrics %>%
  filter(.metric == "rsq") %>%
  arrange(-mean)

sort_mae <- tune_metrics %>%
  filter(.metric == "mae") %>%
  arrange(mean)

# define tuned gradient boosting model based on results above
gb_mod_methane_5kt <- boost_tree(
  trees = 808,         
  tree_depth = 6,    
  min_n = 5,     
  learn_rate = 5.299168e-02
) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

# define workflow
workflow_gb_methane_5kt <- workflow() %>%
  add_model(gb_mod_methane_5kt) %>%
  add_formula(methane_emissions ~ destinations2 + year + propulsion + capacity + delivered)

# fit resamples using the workflow
set.seed(123)
cv_results_gb_methane_5kt <- fit_resamples(
  workflow_gb_methane_5kt,
  resamples = cv_splits,
  metrics = metric_set(rmse, rsq, mae)
)
# collect and print metrics
cv_metrics_gb <- collect_metrics(cv_results_gb_methane_5kt)
print(cv_metrics_gb)
# A tibble: 3 × 6
# .metric .estimator   mean     n std_err .config             
# <chr>   <chr>       <dbl> <int>   <dbl> <chr>               
# 1 mae     standard   16.4       5  0.405  Preprocessor1_Model1
# 2 rmse    standard   29.4       5  1.50   Preprocessor1_Model1
# 3 rsq     standard    0.814     5  0.0164 Preprocessor1_Model1

# put the prediction errors in context
ggplot(emissions_5kt, aes(x=methane_emissions)) + geom_histogram()
summary(emissions_5kt$methane_emissions)

# fit the final model
set.seed(123)
gb_fit_methane_5kt <- fit(workflow_gb_methane_5kt, data = emissions_5kt)

# variable importance plot
vip(gb_fit_methane_5kt)

################################
# carbon dioxide emissions

# define workflow to train model on calculated carbon dioxide emissions
workflow_gb <- workflow() %>%
  add_model(gb_mod) %>%
  add_formula(co2_emissions ~ destinations2 + year + propulsion + capacity + delivered)

# tune the model using cross-validation
set.seed(123)
tune_results <- tune_grid(
  workflow_gb,              
  resamples = cv_splits, 
  grid = grid,
  metrics = metric_set(rmse, rsq, mae)  # evaluation metrics
)


# collect and assess the results
tune_metrics <- tune_results %>%
  collect_metrics()

sort_rsme <- tune_metrics %>%
  filter(.metric == "rmse") %>%
  arrange(mean)

sort_rsq <- tune_metrics %>%
  filter(.metric == "rsq") %>%
  arrange(-mean)

sort_mae <- tune_metrics %>%
  filter(.metric == "mae") %>%
  arrange(mean)

# define tuned gradient boosting model based on results above
gb_mod_co2_5kt <- boost_tree(
  trees = 808,         
  tree_depth = 6,    
  min_n = 5,     
  learn_rate = 5.299168e-02
) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

# define workflow
workflow_gb_co2_5kt <- workflow() %>%
  add_model(gb_mod_co2_5kt) %>%
  add_formula(co2_emissions ~ destinations2 + year + propulsion + capacity + delivered)

# fit resamples using the workflow
set.seed(123)
cv_results_gb_co2_5kt <- fit_resamples(
  workflow_gb_co2_5kt,
  resamples = cv_splits,
  metrics = metric_set(rmse, rsq, mae)
)
# collect and print metrics
cv_metrics_gb <- collect_metrics(cv_results_gb_co2_5kt)
print(cv_metrics_gb)
# A tibble: 3 × 6
# .metric .estimator     mean     n std_err .config             
# <chr>   <chr>         <dbl> <int>   <dbl> <chr>               
#   1 mae     standard   1035.        5 35.3    Preprocessor1_Model1
# 2 rmse    standard   1654.        5 82.2    Preprocessor1_Model1
# 3 rsq     standard      0.773     5  0.0158 Preprocessor1_Model1

# put the prediction errors in context
ggplot(emissions_5kt, aes(x=co2_emissions)) + geom_histogram()
summary(emissions_5kt$co2_emissions)

# fit the final model
set.seed(123)
gb_fit_co2_5kt <- fit(workflow_gb_co2_5kt, data = emissions_5kt)

# variable importance plot
vip(gb_fit_co2_5kt)

# clean up environment
rm(cv_metrics_gb, workflow_gb_methane_5kt,gb_mod_methane_5kt,workflow_gb_co2_5kt,gb_mod_co2_5kt, workflow_gb, tune_results, tune_metrics, sort_mae, sort_rsme, sort_rsq,cv_results_gb_co2_5kt,cv_results_gb_methane_5kt,cv_splits,gb_mod,grid)

#########################
# predicted emissions

missing_emissions <- anti_join(us_exports,emissions_5kt, by = "departure_id") %>%
  select(departure_id,propulsion,delivered,capacity,year) %>%
  inner_join(journey_destinations, by = "departure_id") %>%
  unique()

missing_methane_emissions <- predict(gb_fit_methane_5kt, missing_emissions) %>%
  rename(methane_emissions = .pred)
missing_co2_emissions <- predict(gb_fit_co2_5kt, missing_emissions) %>%
  rename(co2_emissions = .pred)

missing_emissions <- missing_emissions %>%
  bind_cols(missing_methane_emissions) %>%
  bind_cols(missing_co2_emissions) %>%
  mutate(total_emissions_co2_equivalent = co2_emissions + (methane_emissions * 82.5))

#########################
# final emissions calculations

emissions_5kt <- bind_rows(emissions_5kt, missing_emissions)

emissions_5kt_year <- emissions_5kt %>%
  group_by(year) %>%
  summarize(
    export_journeys = n_distinct(departure_id),
    methane_emissions = sum(methane_emissions),
    co2_emissions = sum(co2_emissions),
    total_emissions_co2_equivalent = sum(total_emissions_co2_equivalent)
  )

rm(emissions_10kt,emissions_10kt_year)

#############################
# modeling of distances for outward journeys
distance_durations <- inner_join(emissions_5kt, durations, by = c("departure_id","year")) 

# convert to categorical variables
distance_durations <- distance_durations %>%
  mutate(year = as.factor(year),
         destinations2 = as.factor(destinations2))
         
# create cross-validation resamples
set.seed(123)
cv_splits <- vfold_cv(distance_durations, v = 5)

# using destinations2
# define recipe
lin_reg_recipe <- recipe(simple_path_distance_out_km_5kt ~ destinations2 + year, data = distance_durations) %>%
  step_interact(~ destinations2:year) %>% 
  step_dummy(all_nominal_predictors())

# specify model
lin_reg_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Create a workflow
lin_reg_workflow <- workflow() %>%
  add_recipe(lin_reg_recipe) %>%
  add_model(lin_reg_spec)

# fit the model with cross-validation
cv_results <- lin_reg_workflow %>%
  fit_resamples(
    resamples = cv_splits,
    metrics = metric_set(mae, rmse, rsq), # specify metrics to evaluate
    control = control_resamples(save_pred = TRUE)
  )

# summarize the results
cv_metrics <- cv_results %>%
  collect_metrics()

print(cv_metrics)
# .metric .estimator     mean     n  std_err .config             
# <chr>   <chr>         <dbl> <int>    <dbl> <chr>               
# 1 mae     standard   1776.        5 33.6     Preprocessor1_Model1
# 2 rmse    standard   2858.        5 97.5     Preprocessor1_Model1
# 3 rsq     standard      0.832     5  0.00885 Preprocessor1_Model1

# fit final model
set.seed(123)
distance_fit <- fit(lin_reg_workflow, data = distance_durations)

# extract model coefficients
distance_coeffs_destinations2 <- tidy(distance_fit$fit$fit)

# to China 2943 km longer in 2023-2024
# to Japan 2815 km longer in 2023-2024
# to Chile 3111  km longer in 2023-2024
# to Taiwan 3664 km longer in 2023-2024

# outwards journeys and their distances by destinations and year
mean_distances_year <- distance_durations %>%
  group_by(year,destinations2) %>%
  summarize(mean_distance = mean(simple_path_distance_out_km_5kt, na.rm = TRUE)) %>%
  pivot_wider(names_from = year, values_from = mean_distance) %>%
  mutate(difference = `2023-2024`-`2017-2018`,
         pc_difference = (`2023-2024`-`2017-2018`)/`2017-2018`*100)

journeys_year <- distance_durations %>%
  group_by(year,destinations2) %>%
  count() %>%
  pivot_wider(names_from = year, values_from = n) %>%
  rename(`2017-2018 journeys` = `2017-2018`,
         `2023-2024 journeys` = `2023-2024`)
  
mean_distances_year <- inner_join(journeys_year,mean_distances_year) %>%
  arrange(-pc_difference)

# calculate change in emissions attributable to change in distance for Taiwan, China, Japan, Chile, based on the first model 
chn_jpn_chl_twn_2023_2024 <- distance_durations %>%
  select(departure_id,year,destinations2,distance_total,delivered,capacity,propulsion,days_docked,days_maneuvering) %>%
  filter((destinations2 == "CHN" | destinations2 == "JPN" | destinations2 == "CHL" | destinations2 == "TWN") & year == "2023-2024") %>%
  mutate(distance_total = case_when(destinations2 == "CHN" ~ distance_total - (2943 * 2),
                                    destinations2 == "JPN" ~ distance_total - (2815 * 2),
                                    destinations2 == "CHL" ~ distance_total - (3111 * 2),
                                    destinations2 == "TWN" ~ distance_total - (3664 * 2)),
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
  ) %>%
  select(departure_id,destinations2,distance_total_adj = distance_total, methane_emissions_adj = methane_emissions,co2_emissions_adj = co2_emissions,total_emissions_co2_equivalent_adj = total_emissions_co2_equivalent)

chn_jpn_chl_twn_2023_2024_add <- inner_join(chn_jpn_chl_twn_2023_2024,emissions_5kt, by = c("departure_id","destinations2")) %>%
  select(destinations2,distance_total, distance_total_adj,methane_emissions,methane_emissions_adj,co2_emissions,co2_emissions_adj,total_emissions_co2_equivalent,total_emissions_co2_equivalent_adj) %>%
  # group_by(destinations2) %>%
  summarize(journeys = n(),
            methane_emissions = sum(methane_emissions, na.rm = TRUE),
            methane_emissions_adj = sum(methane_emissions_adj, na.rm = TRUE),
            co2_emissions = sum(co2_emissions, na.rm = TRUE),
            co2_emissions_adj = sum(co2_emissions_adj, na.rm = TRUE),
            total_emissions_co2_equivalent = sum(total_emissions_co2_equivalent, na.rm = TRUE),
            total_emissions_co2_equivalent_adj = sum(total_emissions_co2_equivalent_adj, na.rm = TRUE)) %>%
  mutate(methane_emissions_add = methane_emissions - methane_emissions_adj,
         co2_emissions_add = co2_emissions - co2_emissions_adj,
         total_emissions_co2_equivalent_add = co2_emissions_add + (82.5 * methane_emissions_add))


rm(cv_metrics,cv_results,cv_splits,lin_reg_workflow,lin_reg_spec,lin_reg_recipe)

###########################
# save processed data

save.image("processed_data/us_exports_total_emissions.RData")
