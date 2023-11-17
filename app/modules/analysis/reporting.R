get_reporting_data_from_bq <-
function() {
    
    # Prediction Data
    reporting_tbl <- dplyr::tbl(
        get_bigquery_connection(dataset = "data_pred"),
        "shelter_occupancy_prediction_distinct"
    ) %>% 
        collect() %>% 
        filter(occupancy_date >= Sys.Date()) %>% 
        filter(pred_rank == 1) %>% 
        mutate(pred_time = lubridate::with_tz(pred_time))
    
    # Return
    return(reporting_tbl)
    
}
