# EXTRACT WEATHER FORECAST SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts_dev"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(bigrquery)

# * Source ----
source(file = "../functions/extract_shelter_data.R")


# *****************************************************************************
# **** ----
# MODULARIZE ----
# *****************************************************************************

# * Get Request URL ----
get_request_url <- function(city_loc = "55488") {
    
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


# * Get Request Contents ----
get_request <- function(response, output = "data") {
    
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
    switch(
        output,
        data    = json$DailyForecasts,
        json    = json,
        content = content,
        stop("Invalid output type.", call. = FALSE)
    )
}


# * Get Data ----
get_data <- function(data) {
    
    # Format data as tibble
    ret <- data %>% 
        as_tibble() %>% 
        janitor::clean_names() %>% 
        select(date, temperature_minimum_value, temperature_maximum_value) %>% 
        rename(temp_min = temperature_minimum_value, temp_max = temperature_maximum_value) %>% 
        mutate(temp_avg = (temp_min + temp_max) / 2) %>% 
        mutate(date = as.Date(lubridate::ymd_hms(date)))
    
    # Metadata
    mtd <- str_glue(
        "Metadata (AccuWeather Forecast Data Extract):
            Last Raw Data Extract Date: {Sys.Date()}
            Date Range for New Data Extact: {min(ret$date)} - {max(ret$date)}
            New Data Rows: {nrow(ret)}
            New Data Cols: {ncol(ret)}
        "
    )
    
    return(list(
        weather_forecast = ret,
        metadata         = mtd
    ))
}

weather_forecast_list <- get_request_url() %>% 
    get_request() %>% 
    get_data()

weather_forecast_tbl <- weather_forecast_list[[1]]



# * Upload to Big Query ----
weather_forecast_bq_upload <- get_bigquery_upload(
    upload_job = "weather",
    values     = weather_forecast_tbl,
    project    = "toronto-shelter-project",
    dataset    = "data_clean",
    table      = "weather_forecast_5_day",
    write_disposition = "WRITE_APPEND"
)

# *****************************************************************************
# **** ----
# COLLECT METADATA ----
# *****************************************************************************

list(
    weather_extract_mtd = weather_forecast_list[[2]],
    weather_upload_mtd  = weather_forecast_bq_upload
) 

# *****************************************************************************
# **** ----
# DESCRIPTION ----
# *****************************************************************************

#' get_request_url();
#' The get_request_url function is designed to retrieve weather data from the AccuWeather API. 
#' It constructs a URL using a #predefined base URL and city location identifier, 
#' appends an API key from the system environment variables, and sends a GET request to the API. 
#' The function returns the API response for further processing. 
#' It does not take any arguments and does not allow customization of the API endpoint or parameters.

#' get_request();
#' The get_request function processes an API response, ensuring it is valid and 
#' extracts relevant data based on the user's specified format. It accepts an API 
#' response and an optional output format parameter (defaulting to "data"). 
#' The function checks the status code of the API response, stops execution and 
#' throws an error message if it's not 200 (HTTP OK). It then extracts content 
#' from the response and converts it from JSON format to a list in R. Depending
#' on the specified output format, it returns either a specific data frame from the list, 
#' the entire list, or the raw content.

#' get_data();
#' The get_data function is tasked with extracting, cleaning, and transforming 
#' weather data from a nested list (presumably obtained from an API like AccuWeather). 
#' It specifically targets temperature and date information, performing several 
#' operations to clean and format this data appropriately. The function creates 
#' two separate tables: one for temperature data and one for date data, which are 
#' subsequently combined. It calculates the average temperature by averaging the 
#' minimum and maximum temperatures and adds this as a new column to the final table. 
#' The function returns a tidy data frame containing cleaned date, minimum temperature, 
#' maximum temperature, and average temperature, ready for further analysis or visualization.



# *****************************************************************************
# **** ----
# SAVE MODULES ----
# *****************************************************************************

# * Save Functions ----
dump(
    list   = c("get_request_url", "get_request", "get_data"),
    file   = "../functions/extract_weather_forecast.R",
    append = FALSE
)
