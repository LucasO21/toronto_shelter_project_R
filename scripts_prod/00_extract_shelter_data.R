# SHELTER DATA EXTRACT (PROD) ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts_prod"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(bigrquery)

# * Source ----
source("../functions/extract_shelter_data.R")

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

shelter_raw_tbl <- get_shelter_data()


# *****************************************************************************
# **** ----
# UPLOAD TO BIGQUERY ----
# *****************************************************************************

upload_job <- get_bigquery_upload(
    values = shelter_raw_tbl[[1]],
    table  = "raw_shelter_2023",
    write_disposition = "WRITE_APPEND"
)
