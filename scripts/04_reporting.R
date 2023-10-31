# REPORTING SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(DT)
library(data.table)
library(bigrquery)
library(tidymodels)
library(h2o)

# * Source ----
source(file = "../functions/extract_shelter_data.R")

# *****************************************************************************
# **** ----
# IMPORT PREDICTIONS ----
# *****************************************************************************

get_reporting_data_from_bq <- function() {
    
    # Prediction Data
    reporting_tbl <- dplyr::tbl(
        get_bigquery_connection(dataset = "data_pred"),
        "data_predictions"
    ) %>% 
        collect() %>% 
        distinct()
    
    return(reporting_tbl)
    
}

reporting_tbl <- get_reporting_data_from_bq()

reporting_tbl %>% glimpse()


# *****************************************************************************
# **** ----
# SAVE FUNCTIONS ----
# *****************************************************************************

dump(
    list = c("get_reporting_data_from_bq"),
    file = "../functions/reporting.R",
    append = FALSE
)


dump(
    list = c("get_reporting_data_from_bq"),
    file = "../app/modules/analysis/reporting.R",
    append = FALSE
)






