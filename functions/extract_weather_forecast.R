get_request_url <-
function(city_loc = "55488") {
    
    # Check API key
    api_key <- Sys.getenv("ACCU_WEATHER_API_KEY")
    if (is.null(api_key) || api_key == "") {
        stop("API key is missing. Set the ACCU_WEATHER_API_KEY environment variable.")
    }
    
    # Build URL
    base_url <- "http://dataservice.accuweather.com/forecasts/v1/daily/5day/"
    url      <-  paste0(base_url, city_loc, "?", "apikey=", api_key)
    
    # API Request
    response <- httr::GET(url)
    
    # Check Response
    if (httr::http_status(response)$category != "Success") {
        warning("API request failed: ", httr::http_status(response)$message)
        return(NULL)
    }
    
    return(response)
}
get_request <-
function(response, output = "data") {
    
    # Validate parameters
    valid_outputs <- c("data", "json", "content")
    if (!output %in% valid_outputs) {
        stop(
            "Invalid output type. Choose from: ", 
            str_c(valid_outputs, collapse = ", "), call. = FALSE
        )
    }
    
    # Check response status
    if (response$status_code != 200) {
        stop(
            str_glue("Status Code: Failed!\nStatus code is {response$status_code}. Should be 200."), 
            call. = FALSE
        )
    }
    
    # Get content
    content <- content(response, as = "text", encoding = "UTF-8")
    
    # Get JSON
    json <- fromJSON(content, flatten = TRUE)
    
    # Select output
    switch(output,
           data = json$DailyForecasts,
           json = json,
           content = content,
           stop("Invalid output type.", call. = FALSE)
    )
}
get_data <-
function(data) {
    
    # Format data as tibble
    ret <- data %>% 
        as_tibble() %>% 
        janitor::clean_names() %>% 
        select(date, temperature_minimum_value, temperature_maximum_value) %>% 
        rename(temp_min = temperature_minimum_value, temp_max = temperature_maximum_value) %>% 
        mutate(temp_avg = (temp_min + temp_max) / 2) %>% 
        mutate(date = as.Date(lubridate::ymd_hms(date)))
    
    return(ret)
}
