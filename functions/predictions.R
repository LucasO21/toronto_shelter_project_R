get_prediction_features_from_bq <-
function(table_name) {
    
    # con
    con <- get_bigquery_connection(dataset = "data_features")
    
    # get shelter features
    shelter_forecast_features_tbl <- dplyr::tbl(con, "shelter_occupancy_forecast_features") %>% 
        collect() %>% 
        mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
        rename(
            capacity_actual = avg_capacity_actual_l7d,
            occupied        = avg_occupied_l7d,
            occupancy_rate  = avg_occupancy_rate_l7d
        ) %>% 
        #mutate(x_id = row_number(), .before = occupancy_date) %>% 
        mutate(occupancy_rate = ifelse(occupancy_rate == 100, "Yes", "No") %>% as.factor()) %>% 
        mutate_if(is_character, as_factor) %>% 
        filter(organization_id %in% c(1, 15, 6)) %>% 
        mutate(across(ends_with("_id"), as.factor))
    
    # get weather features
    weather_forecast_tbl <- dplyr::tbl(con, "feature_weather_forecast") %>% 
        collect()
    
    # distinct shelter forecast dates
    weather_forecast_tbl_2 <- tibble(
        date = unique(shelter_forecast_features_tbl$occupancy_date)
    ) %>% 
        arrange(date) %>% 
        left_join(
            weather_forecast_tbl
        )
    
    if (any(is.na(weather_forecast_tbl_2$temp_min))) {
        
        date_missing <- weather_forecast_tbl_2 %>% filter(is.na(temp_min)) %>% pull(date)
        
        weather_forecast_tbl_final <- weather_forecast_tbl %>% 
            bind_rows(
                tibble(
                    date     = max(date_missing),
                    temp_min = mean(weather_forecast_tbl$temp_min),
                    temp_max = mean(weather_forecast_tbl$temp_max),
                    temp_avg = mean(weather_forecast_tbl$temp_avg)
                )
                
            )
            
    } else {
        weather_forecast_tbl_final <- weather_forecast_tbl_2
    }
    

    
    # message
    message(str_glue(
        "shelter pred data info:
            min date: {min(shelter_forecast_features_tbl$occupancy_date)}
            max date: {max(shelter_forecast_features_tbl$occupancy_date)}
        
        weather forecast data info:
            min date: {min(weather_forecast_tbl_final$date)}
            max date: {max(weather_forecast_tbl_final$date)}
        "
    ))
    
    # return
    return(
        list(
            shelter_forecast_features_tbl = shelter_forecast_features_tbl,
            weather_forecast_tbl          = weather_forecast_tbl_final
        )
    )
    
}
get_prediction_features_combined <-
function(shelter_features, weather_features) {
    
    ret <- shelter_features %>% 
        left_join(
            weather_features,
            by = c("occupancy_date" = "date")
            #relationship = "one-to-many"
        )
    
    return(ret)
}
get_prediction_recipes <-
function(data) {
    
    # h2o init
    h2o.init()
    
    # h2o init check
    if (!h2o::h2o.clusterIsUp()) {
        stop("H2O failed to start.")
    }
    
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
    
    # recipe checks
    if (!inherits(prob_tbl, "H2OFrame")) {
        stop("Data processing for prob_tbl failed.", call. = FALSE)
    }
    
    
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
    
    # LOAD MODELS / MAKE PREDICTIONs
    
    # * Prob Model
    pred_tbl_prob <- h2o.loadModel(
        "../artifacts/h2o_artifacts_v1/prob/StackedEnsemble_AllModels_1_AutoML_1_20231026_60055"
    ) %>% 
        h2o.predict(newdata = list[[1]]) %>% 
        as_tibble() %>% 
        rename(pred_occupancy_rate = predict)
    
    # * Reg Model
    pred_tbl_reg <- h2o.loadModel(
        "../artifacts/h2o_artifacts_v1/reg/StackedEnsemble_BestOfFamily_1_AutoML_2_20231026_61754"
    ) %>% 
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
        # select(occupancy_date, location_id, capacity_actual) %>% 
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
        # select(
        #     x_id, occupancy_date, pkey, starts_with("organization"),
        #     starts_with("shelter"), starts_with("location"), 
        #     program_id, program_name, starts_with("program"), sector, 
        #     overnight_service_type, capacity_type, starts_with("pred"), everything()
        # ) 
    
    # Message
    message(str_glue(
        "prediction range: {min(ret$occupancy_date)} - {max(ret$occupancy_date)}
        prediction time: {unique(ret$pred_time)}"
    ))
    
    # Return
    return(ret)
    
}
