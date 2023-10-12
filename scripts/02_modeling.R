# MODELING SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts"))

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

# * Import Data From BQ ----
get_features_data_from_bigquery <- function(year = 2023) {
  
  con <- get_bigquery_connection(dataset = "data_features")
  
  tables <- DBI::dbListTables(con)
  
  shelter_tbl <- dplyr::tbl(con, str_glue("feature_shelter_plus_weather_{year}")) %>% 
    collect() %>% 
    mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
    mutate(location_address = str_trim(location_address, side = c("both"))) %>% 
    mutate(location_city = str_trim(location_city, side = c("both")))
  
  # weather_tbl <- dplyr::tbl(con, str_glue("feature_weather_{year}")) %>% 
  #   collect() %>% 
  #   mutate(date = lubridate::ymd(date))
  
  # message
  message(
    str_glue(
      "
      Shelter Data Info:
        nrow: {nrow(shelter_tbl)}
        ncol: {ncol(shelter_tbl)}
        min date: {min(shelter_tbl$occupancy_date)}
        max date: {max(shelter_tbl$occupancy_date)}
      "
    )
  )
  
  # return 
  return(shelter_tbl)
  
}

features_2022_tbl <- get_features_data_from_bigquery(year = 2022)

features_2023_tbl <- get_features_data_from_bigquery(year = 2023)

features_2022_tbl %>% sapply(function(x) sum(is.na(x)))

features_2023_tbl %>% sapply(function(x) sum(is.na(x)))


# * Combine Data ----


# * Combine Data Step 2: 2022 & 2023 ----
daily_combined_tbl <- features_2022_tbl %>% 
  bind_rows(features_2023_tbl)


# *****************************************************************************
# **** ----
# EDA ----
# *****************************************************************************

# * Check For Nulls ----
# daily_shelter_tbl %>% 
#   sapply(function(x) sum(is.na(x)))
# 
# # * Distinct Counts ----
# 
# # * Distinct Organizations
# daily_shelter_tbl %>% distinct(organization_id) %>% nrow()
# 
# # * Distinct Shelters
# daily_shelter_tbl %>% distinct(shelter_id) %>% nrow()
# 
# # * Distinct Locations
# daily_shelter_tbl %>% distinct(location_id) %>% nrow()
# 
# # * Distinct Programs
# daily_shelter_tbl %>% distinct(program_id) %>% nrow()
# 
# # * Others
# daily_shelter_tbl %>% distinct(overnight_service_type)
# 
# 
# # * Actual Occupancy by Type ----
# daily_shelter_tbl %>% 
#   count(program_model, sort = TRUE) %>% 
#   mutate(pct = n/sum(n))
# 
# 
# # * Actual Occupancy Shelter ----
# daily_shelter_tbl %>% 
#   count(shelter_id, sort = TRUE) %>% 
#   mutate(pct = n/sum(n)) %>% 
#   mutate(pct_cum = cumsum(pct)) %>% 
#   head(10)
# 
# daily_shelter_tbl %>% 
#   filter(shelter_id == 24) %>% View()
# 
# daily_shelter_tbl %>% 
#   select(capacity_type, program_area) %>% 
#   distinct()


# *****************************************************************************
# **** ----
# CHECK LOCATION IDS ----
# *****************************************************************************

# * Top Orgs by Location ----
daily_combined_tbl %>% 
  filter(occupancy_date >= as.Date("2022-10-01")) %>% 
  summarise(
    n   = n_distinct(location_id),
    .by = c(organization_name, organization_id)
  ) %>% 
  arrange(desc(n))

# *****************************************************************************
# **** ----
# FILTER DOWN DATA FOR TESTING ----
# *****************************************************************************

#' test modeling with 3 organizations (1, 15 and 6)

# * Filter for Top Orgs ----
cohort_tbl <- daily_combined_tbl %>% 
  filter(organization_id %in% c(1, 15, 6)) %>% 
  filter(modeling_cohort == 1) %>% 
  filter(occupancy_date >= as.Date("2022-10-01"))

cohort_tbl %>% sapply(function(x) sum(is.na(x)))

cohort_tbl %>% distinct(location_address) %>% nrow()

cohort_tbl %>% distinct(location_id) %>% nrow()

cohort_tbl %>% glimpse()

# *****************************************************************************
# **** ----
# GET MODELING FEATURES ----
# *****************************************************************************

#' Extract features needed for modeling

# * Modeling Data ----
modeling_tbl <- cohort_tbl %>% 
    select(
      x_id, occupancy_date, organization_id, shelter_id, location_id, program_id,
      sector, program_model, overnight_service_type, program_area, capacity_type, 
      occupied, capacity_actual, occupancy_rate, starts_with("temp")
    ) %>% 
    mutate(occupancy_rate = ifelse(occupancy_rate == 100, "Yes", "No") %>% as.factor()) %>% 
    mutate_if(is_character, as_factor) %>% 
    mutate(organization_id = as.factor(organization_id)) %>% 
    mutate(shelter_id = as_factor(shelter_id)) %>% 
    mutate(location_id = as_factor(location_id)) %>% 
    mutate(program_id = as_factor(program_id))

modeling_tbl %>% glimpse()

# *****************************************************************************
# **** ----
# MODELING DATA PREP  ----
# *****************************************************************************


# * Data Splitting ----
# set.seed(123)
# splits_spec <- initial_split(modeling_tbl, prop = 0.80, strata = occupancy_rate)

# train_raw_tbl <- training(splits_spec)
# test_raw_tbl  <- testing(splits_spec)

train_raw_tbl <- modeling_tbl %>% 
  filter(occupancy_date < as.Date("2023-06-01"))

test_raw_tbl <- modeling_tbl %>% 
  filter(! occupancy_date %in% train_raw_tbl$occupancy_date)


# *****************************************************************************
# **** ----
# RECIPES ----
# *****************************************************************************

# * Recipe Classification ----
get_automl_recipes <- function(data, prob = TRUE) {
  
  if (prob) {
    formula <- recipe(occupancy_rate ~ ., data = data)
  } else {
    formula <- recipe(occupied ~ ., data = data)
  }
  
  recipe_spec <- formula %>% 
    update_role(x_id, new_role = "indicator") %>% 
    step_mutate(capacity_type = case_when(
      str_detect(tolower(capacity_type), "bed") ~ "Bed",
      TRUE                                      ~ "Room"
    ) %>% as_factor) %>% 
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
  
  # recipe_spec <- recipe_spec %>% prep()
  
  return(recipe_spec)
  
  
}

get_automl_recipes(train_raw_tbl, FALSE) %>% prep() %>% juice() %>% glimpse()

recipe_spec_prob <- get_automl_recipes(train_raw_tbl)

recipe_spec_reg <- get_automl_recipes(train_raw_tbl, prob = FALSE)


# *****************************************************************************
# **** ----
# MODEL TRAINING ----
# *****************************************************************************

# * H2o Init ----
h2o.init()

# * H2o AutoMLO ----
get_automl_models <- function(recipe, train_data, test_data, mrspm = 300, 
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

# * Automl Models Reg ----
automl_output_list_reg <- get_automl_models(
  recipe     = recipe_spec_reg,
  train_data = train_raw_tbl,
  test_data  = test_raw_tbl,
  prob       = FALSE
)

# * View Outputs
automl_output_list_prob[1]

automl_output_list_reg[1]

# Save Output
automl_output_list_prob %>% write_rds("../artifacts/h2o_models_v1/automl_list_prob.rds")

automl_output_list_reg %>% write_rds("../artifacts/h2o_models_v1/automl_list_reg.rds")


# * Leaderboards ----

# ** Leaderboard Prob ----
leaderboard_h2o_prob <- automl_output_list_prob$h2o_automl_models

# ** Leaderboard Reg ----
leaderboard_h2o_reg <- automl_output_list_reg$h2o_automl_models


# * Making Predictions ----


# * Performance ----


# *****************************************************************************
# **** ----
# MLFLOW ----
# *****************************************************************************

# *****************************************************************************
# **** ----
# SAVE FUNCTIONS ----
# *****************************************************************************

# * Save Functions ----
dump(
  list = c(
    "get_features_data_from_bigquery", 
    "get_modeling_features", 
    "get_automl_recipes", 
    "get_automl_models"
    ),
  file = "../functions/modeling.R",
  append = FALSE
)

