# WEATHER FORECAST EXTRACT (PROD) ----
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

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

weather_forecast_tbl <- get_request_url() %>% 
    get_request() %>% 
    get_data()


# *****************************************************************************
# **** ----
# UPLOAD TO BIGQUERY ----
# *****************************************************************************

get_bigquery_upload(
    values  = weather_forecast_tbl,
    project = "toronto-shelter-project",
    dataset = "data_clean",
    table   = "weather_forecast_5_day",
    write_disposition = "WRITE_APPEND"
)
