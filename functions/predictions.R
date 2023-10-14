get_predictions_features_from_bq <-
function(table_name) {
    
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
get_prediction_features_combined <-
function(shelter_features, weather_features) {
    
    ret <- shelter_features %>% 
        left_join(
            weather_features,
            by = c("occupancy_date" = "date"),
            relationship = "many-to-many"
        )
    
    return(ret)
}
get_prediction_recipes <-
function(data) {
    
    # * Prob ----
    prob_tbl <- get_automl_recipes(data) %>% 
        prep() %>% 
        juice() %>% 
        as.h2o()
    
    # * Reg ----
    reg_tbl <- get_automl_recipes(data, prob = FALSE) %>% 
        prep() %>% 
        juice() %>% 
        as.h2o()
    
    # return
    return(
        list(
            data_prep_prob = prob_tbl,
            data_prep_reg  = reg_tbl
        )
    )
    
}
get_predictions <-
function(list) {
    
    # LOAD MODELS
    
    # * Prob Model
    automl_leader_model_prob <- read_rds(
        "../artifacts/h2o_models_v1/automl_list_prob.rds"
    )[[1]]@leader
    
    # * Reg Model
    automl_leader_model_reg <- read_rds(
        "../artifacts/h2o_models_v1/automl_list_reg.rds"
    )[[1]]@leader
    
    # MAKE PREDICTIONS 
    
    # * Prob
    pred_tbl_prob <- automl_leader_model_prob %>% 
        h2o.predict(newdata = list[[1]]) %>% 
        as_tibble() %>% 
        rename(pred_occupancy_rate = predict)
    
    # * Reg
    pred_tbl_reg <- automl_leader_model_reg %>% 
        h2o.predict(newdata = list[[2]]) %>% 
        as_tibble() %>% 
        rename(pred_occupied = predict) %>% 
        mutate(pred_occupied = round(pred_occupied))
    
    # * Combine 
    ret <- bind_cols(pred_tbl_prob, pred_tbl_reg)
    
    # return
    return(ret)
    
}
get_predictions_formatted <-
function(raw_data, pred_data) {
    
    # Current Time
    current_time <- Sys.time()
    formatted_time_est <- format(
        with_tz(current_time, tzone = "America/New_York"), "%Y-%m-%d %I:%M %p"
    )
    
    # Data Formatted
    ret <- raw_data %>% 
        select(occupancy_date, location_id, capacity_actual) %>% 
        bind_cols(pred_data) %>% 
        rename(pred_capacity_actual = capacity_actual) %>% 
        rename(pred_fully_occupied = pred_occupancy_rate) %>% 
        mutate(pred_occupancy_rate = pred_occupied / pred_capacity_actual) %>% 
        mutate(pred_occupied_adj = case_when(
            pred_occupied > pred_capacity_actual ~ pred_capacity_actual,
            TRUE                                 ~ pred_occupied
        )) %>% 
        mutate(pred_available = abs(pred_capacity_actual - pred_occupied_adj)) %>% 
        mutate(pred_occupancy_rate_adj = pred_occupied_adj / pred_capacity_actual) %>% 
        mutate(pred_fully_occupied_adj = case_when(
            pred_available != 0 ~ "No",
            TRUE                ~ "Yes"
        )) %>% 
        mutate(flag = case_when(
            pred_fully_occupied != pred_fully_occupied_adj ~ "No Match",
            TRUE                                           ~ "Match"
        )) %>% 
        mutate(pred_time = formatted_time_est)
    
    # Message
    message(str_glue(
        "prediction range: {min(ret$occupancy_date)} - {max(ret$occupancy_date)}
        prediction time: {unique(ret$pred_time)}"
    ))
    
    # Return
    return(ret)
    
}
