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
        collect()
    
    return(reporting_tbl)
    
}

reporting_tbl <- get_reporting_data_from_bq()

reporting_tbl %>% glimpse()

reporting_tbl %>% View()


# *****************************************************************************
# **** ----
# FORMAT PREDICTIONS ----
# *****************************************************************************

# get_reporting_data <- function(data, location = NULL) {
#     
#     # Filter for Location
#     if (is.null(location)) {
#         location_tbl <- data
#     } else {
#         location_tbl <- reporting_tbl %>% 
#             filter(location_id == location)
#     }
#     
#     # Location Info
#     # location_info <- str_glue(
#     #     "
#     #     Organization Name  {unique(location_tbl$organization_name)}
#     #     Location Name:     {unique(location_tbl$location_name)}
#     #     Location ID:       {unique(location_tbl$location_id)}
#     #     Location Address:  {unique(location_tbl$location_address)}
#     #     Location City:     {unique(location_tbl$location_city)}
#     #     Location Province: {unique(location_tbl$location_province)}
#     #     "
#     # )
#     
#     # Location Predictions
#     data_tbl <- location_tbl %>% 
#         # select(occupancy_date, shelter_group,
#         #        program_name, program_model, program_area, sector,
#         #        overnight_service_type, capacity_type, pred_capacity_actual,
#         #        pred_occupied_adj, pred_available, pred_fully_occupied_adj,
#         #        pred_occupancy_rate_adj) %>% 
#         # mutate(pred_occupancy_rate_adj = scales::percent(pred_occupancy_rate_adj))
#         select(-ends_with("id"), -pkey) %>% 
#         select(occupancy_date:capacity_type, pred_capacity_actual, pred_occupied_adj, 
#                pred_available, pred_fully_occupied_adj, pred_occupancy_rate_adj) %>% 
#         mutate(pred_occupancy_rate_adj = scales::percent(pred_occupancy_rate_adj, accuracy = 0.02))
#     
#     # DT Table 
#     dt_table <- data_tbl %>% 
#         datatable(
#             options = list(
#                 columnDefs = list(
#                     list(className = "dt-center", targets = c(9:13)),
#                     list(width = "200", targets = 3)
#                 )
#             )
#         )
#     
#     # Output
#     # if (is.null(location)) {
#     #     location_tbl <- data
#     # } else {
#     #     location_tbl <- reporting_tbl %>% 
#     #         filter(location_id == location)
#     # }
#     
#     
#     # Return
#     return(dt_table)
#     
#     
# }
# 
# get_reporting_data(reporting_tbl)


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






