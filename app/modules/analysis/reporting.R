
library(bigrquery)
bq_auth(path = "../../toronto-shelter-project-064ce917a6a1.json")

get_reporting_data_from_bq <-
function() {
    
    # Prediction Data
    reporting_tbl <- dplyr::tbl(
        get_bigquery_connection(dataset = "data_pred"),
        "data_predictions"
    ) %>% 
        collect() %>% 
        distinct()
    
    return(reporting_tbl)
    
}
