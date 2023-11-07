# WEATHER FORECAST EXTRACT (PROD) ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd("~/Desktop/School/2023_projects/toronto_shelter_project/scripts_prod")

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

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

weather_forecast_list <- get_request_url() %>% 
    get_request() %>% 
    get_data()

weather_forecast_tbl <- weather_forecast_list[[1]]


# *****************************************************************************
# **** ----
# UPLOAD TO BIGQUERY ----
# *****************************************************************************

weather_data_upload_job <- get_bigquery_upload(
    upload_job = "weather",
    values     = weather_forecast_tbl,
    project    = "toronto-shelter-project",
    dataset    = "data_clean",
    table      = "weather_forecast_5_day",
    write_disposition = "WRITE_APPEND"
)


# *****************************************************************************
# **** ----
# METADATA ----
# *****************************************************************************

list(
    weather_extract_mtd = weather_forecast_list[[2]],
    weather_upload_mtd  = weather_data_upload_job
) %>% 
    write_rds("../app/artifacts/weather_metadata_list.rds")
