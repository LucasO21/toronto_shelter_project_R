# PREDICTIONS (PROD) ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("prod_scripts"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(bigrquery)
library(httr)
library(jsonlite)

# * Source ----
source("../functions/extract_shelter_data.R")
source("../functions/extract_weather_forecast.R")
source("../functions/predictions.R")
source("../functions/modeling.R")

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

# * Get Data From BQ ----
pred_features_data_list <- get_prediction_features_from_bq()


# * Combine ----
pred_features_combined_tbl <- get_prediction_features_combined(
    pred_features_data_list[[1]], 
    pred_features_data_list[[2]]
) %>% 
    select(
        occupancy_date, ends_with("_id"), occupied, capacity_actual, occupancy_rate
    ) %>% 
    mutate(across(ends_with("_id"), ~ as.factor(.)))


# * Data Processing ----
pred_data_processed_list <- get_prediction_recipes(pred_features_combined_tbl)


# * Make Predictions ----
predictions_tbl <- get_predictions(pred_data_processed_list)


# * Format Predictions ----
predictions_final_tbl <- get_predictions_formatted(
    raw_data  = pred_features_data_list[[1]],
    pred_data = predictions_tbl
)


# *****************************************************************************
# **** ----
# UPLOAD TO BIGQUERY ----
# *****************************************************************************

get_bigquery_upload(
    values  = predictions_final_tbl,
    project = "toronto-shelter-project",
    dataset = "data_pred",
    table   = "data_predictions",
    write_disposition = "WRITE_APPEND"
)
