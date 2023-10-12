# PREDICTION SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(bigrquery)
library(tidymodels)
library(h2o)

# * Source ----
source(file = "../functions/extract_shelter_data.R")

# *****************************************************************************
# **** ----
# DATA IMPORT ----
# *****************************************************************************

# * BQ Connection ----
con <- get_bigquery_connection(dataset = "data_features")

# * List Tables ----
dbListTables(con)

# * Get Data ----
get_pred_features_from_bq <- function(table_name) {
    
    # con
    con <- get_bigquery_connection(dataset = "data_features")
    
    # get shelter features
    shelter_pred_tbl <- dplyr::tbl(con, "feature_pred_shelter") %>% 
        collect() %>% 
        mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
        rename(
            capacity_actual = avg_capacity_actual_l7d,
            occupied        = avg_occupied_l7d,
            occupancy_rate  = avg_occupancy_rate_l7d
        ) %>% 
        mutate(x_id = row_number(), .before = occupancy_date) %>% 
        mutate(occupancy_rate = ifelse(occupancy_rate == 100, "Yes", "No") %>% as.factor()) %>% 
        mutate_if(is_character, as_factor) %>% 
        filter(organization_id %in% c(1, 15, 6)) %>% 
        mutate(across(c(organization_id, shelter_id, location_id, program_id), as.factor))
    
    # get weather features
    weather_forecast_tbl <- dplyr::tbl(con, "feature_weather_forecast") %>% 
        collect()
    
    weather_forecast_tbl <- weather_forecast_tbl %>% 
        bind_rows(
            tibble(
                date     = max(weather_forecast_tbl$date + 1),
                temp_min = mean(weather_forecast_tbl$temp_min),
                temp_max = mean(weather_forecast_tbl$temp_max),
                temp_avg = mean(weather_forecast_tbl$temp_avg)
            )
            
        )
    
    # message
    message(str_glue(
        "shelter pred data info:
            min date: {min(shelter_pred_tbl$occupancy_date)}
            max date: {max(shelter_pred_tbl$occupancy_date)}
        ==============================================================
        weather forecast data info:
            min date: {min(weather_forecast_tbl$date)}
            max date: {max(weather_forecast_tbl$date)}
        "
    ))
    
    # return
    return(
        list(
            shelter_pred_tbl     = shelter_pred_tbl,
            weather_forecast_tbl = weather_forecast_tbl
        )
    )
    
}

pred_features_data_list <- get_pred_features_from_bq()


# *****************************************************************************
# **** ----
# COMBINE DATA ----
# *****************************************************************************

# * Combine Shelter & Weather Features ----
get_pred_features_combined <- function(shelter_features, weather_features) {
    
    ret <- shelter_features %>% 
        left_join(
            weather_features,
            by = c("occupancy_date" = "date")
        )
    
    return(ret)
}

pred_features_combined <- get_pred_features_combined(
    pred_features_data_list[[1]], 
    pred_features_data_list[[2]]
)



# *****************************************************************************
# **** ----
# PREP DATA (RECIPES) ----
# *****************************************************************************

# * Prob ----
pred_features_tbl_prob <- get_automl_recipes(shelter_pred_tbl) %>% 
    prep() %>% 
    juice() %>% 
    as.h2o()

# * Reg ----
pred_features_tbl_reg <- get_automl_recipes(shelter_pred_tbl, prob = FALSE) %>% 
    prep() %>% 
    juice() %>% 
    as.h2o()

# *****************************************************************************
# **** ----
# LOAD MODELS ----
# *****************************************************************************

# * Prob Model ----
automl_leader_model_prob <- read_rds(
    "../artifacts/h2o_models_v1/automl_list_prob.rds"
)[[1]]@leader

# * Reg Model ----
automl_leader_model_reg <- read_rds(
    "../artifacts/h2o_models_v1/automl_list_reg.rds"
)[[1]]@leader


# *****************************************************************************
# **** ----
# MAKE PREDICTIONS ----
# *****************************************************************************

# * Prob ----
pred_tbl_prob <- automl_leader_model_prob %>% 
    h2o.predict(newdata = pred_features_tbl_prob) %>% 
    as_tibble() %>% 
    rename(pred_occupancy_rate = predict)


# * Reg ----
pred_tbl_reg <- automl_leader_model_reg %>% 
    h2o.predict(newdata = pred_features_tbl_reg) %>% 
    as_tibble() %>% 
    rename(pred_occupied = predict) %>% 
    mutate(pred_occupied = round(pred_occupied))


# *****************************************************************************
# **** ----
# FORMAT PREDICTIONS ----
# *****************************************************************************

# * Format Predictions ----
pred_formated_tbl <- shelter_pred_tbl %>% 
    select(occupancy_date, location_id, capacity_type, capacity_actual,
           occupied, occupancy_rate) %>% 
    bind_cols(pred_tbl_prob) %>% 
    bind_cols(pred_tbl_reg) %>% 
    select(-occupancy_rate, -occupied) %>% 
    rename(avg_capacity_actual = capacity_actual) %>% 
    rename(pred_fully_occupied = pred_occupancy_rate) %>% 
    mutate(pred_occupancy_rate = pred_occupied / avg_capacity_actual) %>% 
    mutate(pred_occupied_adj = case_when(
        pred_occupied > avg_capacity_actual ~ avg_capacity_actual,
        TRUE                                ~ pred_occupied
    )) %>% 
    mutate(pred_available = abs(avg_capacity_actual - pred_occupied_adj)) %>% 
    mutate(pred_occupancy_rate_adj = pred_occupied_adj / avg_capacity_actual)


# *****************************************************************************
# **** ----
# UPLOAD PREDICTIONS TO BIG QUERY ----
# *****************************************************************************

# * Prob ----
get_bigquery_upload(
    values  = pred_formated_tbl,
    project = "toronto-shelter-project",
    dataset = "data_pred",
    table   = "data_predictions",
    write_disposition = "WRITE_APPEND"
)
