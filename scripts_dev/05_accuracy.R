# ACCURACY ----
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

# * Source ----
source(file = "../functions/extract_shelter_data.R")


# *****************************************************************************
# **** ----
# CONNECTION ----
# *****************************************************************************

# * Connect to BigQuery ----
con <- get_bigquery_connection(dataset = "data_pred")

# * List Tables ----
dbListTables(con)


# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

# * Predictions ----
predictions_tbl <- dplyr::tbl(
    con,
    "shelter_occupancy_prediction_distinct"
) %>% 
    collect() %>% 
    mutate(occupancy_date = lubridate::ymd(occupancy_date))

predictions_tbl %>% distinct(pkey)

# * Pkeys with Prediction List ----
pkey_with_prediction_list <- predictions_tbl %>% 
    distinct(pkey) %>% 
    pull()

# * Actual Data ----
actual_tbl <- dplyr::tbl(
    get_bigquery_connection(dataset = "data_clean"),
    "shelter_occupancy_2022_2023_flagged_id"
) %>% 
    collect() %>% 
    mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
    filter(occupancy_date >= as.Date("2023-11-01"))


# *****************************************************************************
# **** ----
# SAMPLE LOCATION ----
# *****************************************************************************

# * Predictions ----
sample_location_pred_tbl <- predictions_tbl %>% 
    filter(pkey == "1-2-1066-16231-5-1-1-1-1")

sample_location_pred_tbl %>% glimpse()

# * Actual ----
sample_actual_tbl <- actual_tbl %>% 
    filter(pkey == "15-25-1167-15492-1-1-1-1-1") %>% 
    arrange(occupancy_date) %>% 
    View()

"1-3-1008-17071-4-1-3-3-2" 

min(predictions_tbl$occupancy_date)


pkey_with_actual_tbl <- actual_tbl %>% 
    filter(occupancy_date >= as.Date("2023-11-01")) %>% 
    filter(pkey %in% pkey_with_prediction_list) %>% 
    select(pkey, occupancy_date, occupied)

predictions_tbl %>% 
    select(-occupied) %>% 
    left_join(
        pkey_with_actual_tbl,
        by = c("occupancy_date" = "occupancy_date", "pkey" = "pkey")
    ) %>% 
    glimpse()
    


actual_tbl %>% 
    distinct(occupancy_date) %>% 
    mutate(day = lubridate::wday(occupancy_date, label = T)) %>% 
    distinct(day)


# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
