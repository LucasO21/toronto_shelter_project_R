get_features_data_from_bigquery <-
function(year = 2023) {
  
  con <- get_bigquery_connection(dataset = "data_features")
  
  tables <- DBI::dbListTables(con)
  
  shelter_tbl <- dplyr::tbl(con, str_glue("feature_shelter_plus_weather_{year}")) %>% 
    collect() %>% 
    mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
    mutate(location_address = str_trim(location_address, side = c("both"))) %>% 
    mutate(location_city = str_trim(location_city, side = c("both")))
  
  # weather_tbl <- dplyr::tbl(con, str_glue("feature_weather_{year}")) %>% 
  #   collect() %>% 
  #   mutate(date = lubridate::ymd(date))
  
  # message
  message(
    str_glue(
      "
      Shelter Data Info:
        nrow: {nrow(shelter_tbl)}
        ncol: {ncol(shelter_tbl)}
        min date: {min(shelter_tbl$occupancy_date)}
        max date: {max(shelter_tbl$occupancy_date)}
      "
    )
  )
  
  # return 
  return(shelter_tbl)
  
}
