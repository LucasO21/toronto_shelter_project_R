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
con <- get_bigquery_connection(dataset = "data_pred")

# * List Tables ----
dbListTables(con)

# * Load Data ----
shelter_pred_tbl <- dplyr::tbl(con, "pred_shelter") %>% 
    collect() %>% 
    mutate(forecast_date = lubridate::ymd(forecast_date)) %>% 
    rename(occupancy_date = forecast_date) %>% 
    rename(
        capacity_actual = avg_capacity_actual_l7d,
        occupied        = avg_occupied_l7d,
        occupancy_rate  = avg_occupancy_rate_l7d
    ) %>% 
    mutate(x_id = row_number(), .before = occupancy_date) %>% 
    mutate(occupancy_rate = ifelse(occupancy_rate == 100, "Yes", "No") %>% as.factor()) %>% 
    mutate_if(is_character, as_factor) %>% 
    mutate(organization_id = as.factor(organization_id)) %>% 
    mutate(shelter_id = as_factor(shelter_id)) %>% 
    mutate(location_id = as_factor(location_id)) %>% 
    mutate(program_id = as_factor(program_id)) %>% 
    filter(organization_id %in% c(1, 15, 6))

shelter_pred_tbl %>% glimpse()



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
