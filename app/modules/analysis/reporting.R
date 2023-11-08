get_reporting_data_from_bq <-
function() {
    
    # Prediction Data
    reporting_tbl <- dplyr::tbl(
        get_bigquery_connection(dataset = "data_pred"),
        "shelter_occupancy_prediction_distinct"
    ) %>% 
        collect() %>% 
        distinct()
    
    return(reporting_tbl)
    
}
