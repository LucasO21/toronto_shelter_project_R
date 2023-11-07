# REPORTING SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts_dev"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(DT)
library(data.table)
library(bigrquery)
library(tidymodels)
library(h2o)
library(leaflet)
library(tidygeocoder)

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
# GEOCODE ----
# *****************************************************************************

# * Geocode ----
reporting_geocoded_tbl <- reporting_tbl %>% 
    mutate(address = str_glue(
        "{location_address}, {location_city}, {location_province}, {location_postal_code}"
    )) %>% tidygeocoder::geocode(
        address = address,
        method  = "arcgis"
    ) 

reporting_geocoded_tbl %>% glimpse()

reporting_geocoded_tbl %>% 
    summarise(
        count_of_programs   = n_distinct(program_id),
        mean_occupancy_rate = mean(pred_occupancy_rate_adj),
        .by = location_id
    ) 

# * Save Geocode ----
reporting_geocoded_tbl %>% 
    select(location_id, lat, long) %>% 
    distinct() %>% 
    write_rds("../app/modules/data/geocode.rds")

# *****************************************************************************
# **** ----
# MAP ----
# *****************************************************************************

# * Data ----
reporting_geocoded_tbl2 <- reporting_geocoded_tbl %>% 
    left_join(
        reporting_geocoded_tbl %>% 
            summarise(
                count_of_programs   = n_distinct(program_id),
                mean_occupancy_rate = mean(pred_occupancy_rate_adj),
                .by = location_id
            ) 
    ) 

# * Location Name ----
location_name_list <- reporting_geocoded_tbl2$location_name

# * Color ----
get_color <- function(reporting_geocoded_tbl2) {
    sapply(reporting_geocoded_tbl2$mean_occupancy_rate, function(mean_occupancy_rate) {
        if (mean_occupancy_rate >= 0.9) {
            "red"
        } else if (mean_occupancy_rate < 0.9 & mean_occupancy_rate >= 0.8) {
            "orange"
        } else {
            "green"
        }
    })
}

# * Icons ----
icons <- awesomeIcons(
    icon        = "ios-close",
    iconColor   = "black",
    library     = "ion",
    markerColor = get_color(reporting_geocoded_tbl2)
)

# * Map ----
reporting_geocoded_tbl2 %>% 
    mutate(popup = paste(
        "Location Name: ", location_name, "<br>",
        "Location ID: ", location_id, "<br>",
        "Location Address: ", address, "<br>",
        "Number of Available Programs: ", count_of_programs, "<br>",
        "Mean Occupancy Rate: ", mean_occupancy_rate, "<br>"
    )) %>% 
    leaflet() %>% 
    addTiles() %>% 
    addAwesomeMarkers(
        lng   = ~long,
        lat   = ~lat,
        icon  = icons,
        popup = ~popup,
        group = ~location_name_list
    )

reporting_geocoded_tbl %>% 
    filter(location_id == "1150") %>% 
    View()

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






