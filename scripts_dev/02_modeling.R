# MODELING SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts_dev"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(timetk)
library(bigrquery)
library(tidymodels)
library(rules)
library(vip)
library(tictoc)
library(future)
library(doFuture)
library(parallel)
library(h2o)

# * Source ----
source(file = "../functions/extract_shelter_data.R")

# *****************************************************************************
# **** ----
# CONNECTION ----
# *****************************************************************************

# * Connect to BigQuery ----
con <- get_bigquery_connection(dataset = "data_features")


# * List Tables ----
dbListTables(con)

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

shelter_occupancy_tbl <- dplyr::tbl(con, "shelter_occupancy_weather_integration_2022_2023") %>% 
  collect() %>% 
  mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
  mutate(location_address = str_trim(location_address, side = c("both"))) %>% 
  mutate(location_city = str_trim(location_city, side = c("both")))

shelter_occupancy_tbl %>% glimpse()


# *****************************************************************************
# **** ----
# CHECK LOCATION IDS ----
# *****************************************************************************

# * Top Orgs by Location ----
# - checking to see what organization has the highest number of distinct locations
# shelter_occupancy_tbl %>% 
#   filter(occupancy_date >= as.Date("2022-10-01")) %>% 
#     summarise(
#         n   = n_distinct(location_id),
#         .by = c(organization_name, organization_id)
#     ) %>% 
#     arrange(desc(n))

# - city of toronto, homes first society and dixon hall have the most locations
# - we'll focus on modeling/predicting shelter occupancy for these 3 organizations

# *****************************************************************************
# **** ----
# FILTER DOWN DATA FOR TESTING ----
# *****************************************************************************

# * Filter for Org 1, 15 and 6 ----
analysis_cohort_tbl <- shelter_occupancy_tbl %>% 
    filter(organization_id %in% c(1, 15, 6)) %>% 
    filter(model_cohort_adj == 1)

analysis_cohort_tbl %>% sapply(function(x) sum(is.na(x)))

# *****************************************************************************
# **** ----
# GET MODELING FEATURES ----
# *****************************************************************************

# * Modeling Features ----
modeling_tbl <- analysis_cohort_tbl %>% 
  select(
    occupancy_date, organization_id, shelter_id, location_id, program_id,
    sector_id, program_model_id, overnight_service_type_id, program_area_id, 
    capacity_type_id, occupied, capacity_actual, occupancy_rate, temp_min,
    temp_max, temp_avg
  ) %>% 
  mutate(occupancy_rate = ifelse(occupancy_rate == 100, "Yes", "No") %>% as.factor()) %>% 
  mutate_if(is_character, as_factor) %>% 
  mutate(across(ends_with("_id"), ~ as.factor(.))) %>% 
  filter(occupancy_date %>% between(as.Date("2022-10-01"), as.Date("2023-09-30")))

modeling_tbl %>% glimpse()


# *****************************************************************************
# **** ----
# MODELING PREP ----
# *****************************************************************************


# * Data Splitting ----
train_raw_tbl <- modeling_tbl %>% filter(occupancy_date <= as.Date("2023-06-30"))
test_raw_tbl  <- modeling_tbl %>% filter(! occupancy_date %in% train_raw_tbl$occupancy_date)

min(train_raw_tbl$occupancy_date)
max(train_raw_tbl$occupancy_date)

min(test_raw_tbl$occupancy_date)
max(test_raw_tbl$occupancy_date)


# * Recipes ----

# * Recipe Classification ----
get_automl_recipes <- function(data, prob = TRUE) {
  
  if (prob) {
    formula <- recipe(occupancy_rate ~ ., data = data)
  } else {
    formula <- recipe(occupied ~ ., data = data)
  }
  
  recipe_spec <- formula %>% 
    step_novel(all_nominal(), -all_outcomes()) %>% 
    step_zv(all_predictors()) %>% 
    step_timeseries_signature(occupancy_date) %>% 
    step_rm(matches(".xts|.lbl|.iso|index.num|am.pm|hour|minute|second|hour12")) %>% 
    step_rm(occupancy_date, capacity_actual)
  
  if (prob) {
    recipe_spec <- recipe_spec %>% 
      step_rm(occupied)
  } else {
    recipe_spec <- recipe_spec %>% 
      step_rm(occupancy_rate)
  }
  
  return(recipe_spec)
  
}

get_automl_recipes(train_raw_tbl, TRUE) %>% prep() %>% juice() %>% glimpse()

recipe_spec_prob <- get_automl_recipes(train_raw_tbl)

recipe_spec_reg <- get_automl_recipes(train_raw_tbl, prob = FALSE)
    

# *****************************************************************************
# **** ----
# MODEL TRAINING ----
# *****************************************************************************

# * H2o Init ----
h2o.init()

# * H2o AutoML ----
get_automl_models <- function(recipe, train_data, test_data, mrspm = 60, 
                              seed = 123, prob = TRUE) {
  
  # data processing
  train_prep_tbl <- recipe %>% prep() %>% bake(new_data = train_data)
  test_prep_tbl  <- recipe %>% prep() %>% bake(new_data = test_data)
  
  # target / predictor setup
  if (prob) {
    y <- "occupancy_rate"
    x <- setdiff(names(train_prep_tbl), y)
  } else {
    y <- "occupied"
    x <- setdiff(names(train_prep_tbl), y)
  }
  
  # data as h2o objects
  train_h2o <- as.h2o(train_prep_tbl)
  test_h2o  <- as.h2o(test_prep_tbl)
  
  # h2o automl models
  automl_models_h2o <- h2o.automl(
    x                          = x,
    y                          = y,
    training_frame             = train_h2o,
    validation_frame           = test_h2o,
    max_runtime_secs_per_model = mrspm,
    nfolds                     = 5,
    max_models                 = 10,
    exclude_algos              = c("DeepLearning"),
    seed                       = seed
  )

  return(
    list(
      h2o_automl_models = automl_models_h2o,
      h2o_train         = train_h2o,
      h2o_test          = test_h2o,
      train_raw         = train_prep_tbl,
      test_raw          = test_prep_tbl
    )
  )
  
}

# * Automl Models Prob ----
automl_output_list_prob <- get_automl_models(
  recipe     = recipe_spec_prob,
  train_data = train_raw_tbl,
  test_data  = test_raw_tbl,
  prob       = TRUE
)

automl_output_list_prob[[1]]

# * Automl Models Reg ----
automl_output_list_reg <- get_automl_models(
  recipe     = recipe_spec_reg,
  train_data = train_raw_tbl,
  test_data  = test_raw_tbl,
  prob       = FALSE
)

automl_output_list_reg[[1]]


# *****************************************************************************
# **** ----
# SAVE ARTIFACTS ----
# *****************************************************************************

# * Prob ----
h2o.getModel("StackedEnsemble_AllModels_1_AutoML_2_20231110_185951") %>% 
  h2o.saveModel(path = "../artifacts/h2o_artifacts_v2/prob/")

automl_output_list_prob %>% 
  write_rds("../artifacts/h2o_artifacts_v2/prob/prob_list.rds")


# * Reg ----
h2o.getModel("StackedEnsemble_BestOfFamily_1_AutoML_3_20231110_191540") %>% 
  h2o.saveModel(path = "../artifacts/h2o_artifacts_v2/reg/")

automl_output_list_reg %>% 
  write_rds("../artifacts/h2o_artifacts_v2/reg/reg_list.rds")



# *****************************************************************************
# **** ----
# LEADERBOARD ----
# *****************************************************************************



# *****************************************************************************
# **** ----
# PREDICTIONS ----
# *****************************************************************************


# *****************************************************************************
# **** ----
# PERFORMANCE ----
# *****************************************************************************


# *****************************************************************************
# **** ----
# MLFLOW ----
# *****************************************************************************

# *****************************************************************************
# **** ----
# SAVE FUNCTIONS ----
# *****************************************************************************
dump(
  list = c("get_automl_recipes", "get_automl_models"),
  file = "../functions/modeling.R",
  append = FALSE
)

