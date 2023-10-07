# EXTRACT SHELTER DATA FROM API SCRIPT ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("scripts"))

# * Libraries ----
library(tidyverse)
library(opendatatoronto)
library(bigrquery)


# *****************************************************************************
# **** ----
# MODULES ----
# *****************************************************************************

# * Extract Shelter Data ----
get_shelter_data <- function(slice = 1) {
    
    # info
    info <- show_package("21c83b32-d5a8-4106-a54f-010dbe49f6f2") %>% 
        list_package_resources() %>% 
        filter(str_to_lower(format) %in% c("csv", "geojson")) %>% 
        filter(! is.na(last_modified)) %>% 
        arrange(desc(last_modified))
    
    # data
    ret <- info %>% 
        slice(slice) %>% 
        get_resource() %>% 
        janitor::clean_names()
    
    return(ret)
}


# * Load to Big Query ----
get_bigquery_upload <- function(values, project = "toronto-shelter-project", 
                               dataset = "data_raw", 
                               table, create_disposition = "CREATE_IF_NEEDED",
                               write_disposition = "WRITE_TRUNCATE") {
    
    # Validate parameters
    stopifnot(
        is.data.frame(values),
        is.character(project), length(project) == 1,
        is.character(dataset), length(dataset) == 1,
        is.character(table), length(table) == 1,
        is.character(create_disposition), length(create_disposition) == 1
    )
    
    # Connect to BigQuery
    con <- DBI::dbConnect(
        bigrquery::bigquery(),
        project = project,
        dataset = dataset,
        billing = project  # Assuming billing to the same project, adjust if different
    )
    
    # Perform upload
    job <- bigrquery::insert_upload_job(
        values  = values,
        project = project,
        dataset = dataset,
        table   = table,
        create_disposition = create_disposition
    )
    
    # Message
    job_time <- tibble(
        creation_time = as.numeric(job$statistics$creationTime),
        start_time = as.numeric(job$statistics$startTime)
    ) %>% 
        mutate(creation_time = as.POSIXct(creation_time / 1000, origin = "1970-01-01", tz = "America/New_York") + 2*3600) %>% 
        mutate(start_time = as.POSIXct(start_time / 1000, origin = "1970-01-01", tz = "America/New_York") + 2*3600)

    msg <- message(
        stringr::str_glue(
            "
            Job Status: {job$status}
            Job ID: {job$jobReference$jobId}
            Job Creation Time: {job_time$creation_time}
            Job Start Time: {job_time$start_time}
            ======================================================
            "
        )
    )
    
    # return(job)
}

# test_tbl <- get_shelter_data(slice = 3) %>% head(5)
# 
# get_bigquery_upload(values = test_tbl, table = "test")

test_tbl <- get_shelter_data(slice = 1) %>% tail(5)


# *****************************************************************************
# **** ----
# DATA FORMATTING ----
# *****************************************************************************

# * Data Formatting ----
daily_shelter_formmatted_tbl <- daily_shelter_tbl %>%
    dtplyr::lazy_dt() %>%
    filter(str_detect(str_to_lower(capacity_type), "room")) %>%
    select(-ends_with("bed"), -ends_with("beds")) %>%
    rename_with(~str_remove_all(.x, "_room"), ends_with("room")) %>%
    rename_with(~str_remove_all(.x, "_rooms"), ends_with("rooms")) %>%
    as_tibble() %>%
    bind_rows(
        daily_shelter_tbl %>%
            dtplyr::lazy_dt() %>%
            filter(str_detect(str_to_lower(capacity_type), "bed")) %>%
            select(-ends_with("room"), -ends_with("rooms")) %>%
            rename_with(~str_remove_all(.x, "_bed"), ends_with("bed")) %>%
            rename_with(~str_remove_all(.x, "_beds"), ends_with("beds")) %>%
            as_tibble()
    )


# *****************************************************************************
# **** ----
# DESCRIPTION ----
# *****************************************************************************

#' get_shelter_data();
#' The get_shelter_data function is designed to retrieve, clean, and transform 
#' shelter data from a specified data source, presumably a package or API that 
#' provides access to various datasets related to shelter information. The function 
#' primarily focuses on extracting data in CSV or GeoJSON format, ensuring that 
#' the datasets are not only available but also recently modified, thereby providing 
#' the most up-to-date information available.
#'
#' The function operates in two main steps:
#' 1. Information Retrieval: It first retrieves metadata about available resources 
#'    using show_package() and list_package_resources(), filtering for relevant 
#'    data formats and ensuring that the data is recent by checking the last_modified 
#'    field. The datasets are then arranged in descending order based on their 
#'    last modification date to prioritize the most recent data.
#' 2. Data Extraction and Cleaning: The function then extracts the specified slice 
#'    of data using slice() and get_resource(), and cleans the variable names 
#'    using janitor::clean_names() to ensure they are syntactically valid and 
#'    easy to work with in R.


#' get_bigquery_upload();
#' The get_bigquery_upload function is designed to facilitate the uploading of data 
#' to Google BigQuery while providing flexibility and robustness in handling various 
#' datasets and destinations. It aims to streamline the process of data upload by 
#' establishing a connection to BigQuery, initiating an upload job, and providing 
#' feedback about the job status and timings.
#'
#' The function performs several key operations:
#' 1. Parameter Validation: It validates the input parameters to ensure they are 
#'    of the correct type and non-empty, preventing unintended operations or unclear 
#'    error messages during the upload process.
#' 2. BigQuery Connection: Establishes a connection to Google BigQuery, specifying 
#'    the project and dataset to be used, and assuming the billing to be charged 
#'    to the specified project.
#' 3. Data Upload: Initiates an upload job to insert the data into the specified 
#'    table in BigQuery, with flexibility in handling table creation disposition.
#' 4. Feedback Provision: Provides feedback about the job status, ID, creation time, 
#'    and start time, aiding in monitoring and debugging.


# *****************************************************************************
# **** ----
# SAVE MODULES ----
# *****************************************************************************

# * Save Modules ----
dump(
    list   = c("get_shelter_data", "get_bigquery_upload"),
    file   = "../functions/extract_shelter_data.R",
    append = FALSE
)
