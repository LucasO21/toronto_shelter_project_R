get_shelter_data <-
function(year = 2023) {
    
    # Big Query Connection
    con <- get_bigquery_connection(dataset = "data_raw")
    
    # Get Last Date in Big Query 
    max_date <- DBI::dbGetQuery(
        con,
        "select max(occupancy_date) from `toronto-shelter-project.data_raw.raw_shelter_2023`"
    )
    
    max_date <- max_date %>% pull() %>% as.Date()

    # Oped Data API Info
    info <- show_package("21c83b32-d5a8-4106-a54f-010dbe49f6f2") %>% 
        list_package_resources() %>% 
        filter(str_to_lower(format) %in% c("csv", "geojson")) %>% 
        filter(! is.na(last_modified)) %>% 
        arrange(desc(last_modified)) %>% 
        mutate(last_modified_year = lubridate::year(last_modified))
    
    if (year == 2023) {
        info <- info %>% 
            filter(! str_detect(name, "2022")) %>% 
            filter(! str_detect(name, "2021")) %>% 
            head(1)
    } else if (year == 2022) {
        info <- info %>% 
            filter(str_detect(name, "2022")) %>% 
            head(1)
    }
        
    # Info Check
    if (is.null(info) || length(info) == 0) {
        stop("No API info extracted! Check API info code chunk", call. = FALSE)
    }
    
    # Data Extract (Open Data API)
    data <- info %>% 
        get_resource() %>% 
        janitor::clean_names() %>% 
        mutate(occupancy_date = lubridate::ymd(occupancy_date))
    
    # Data Check
    if (is.null(data) || length(data) == 0) {
        stop("No data extracted! Check data chunk", call. = FALSE)
    }
    
    # Check: New Data
    if (max(data$occupancy_date) == max_date) {
        stop("Max occupancy date from API matches max data in BigQuery", call. = FALSE)
    }
    
    # Filter API Data > Max Occupancy Date in BQ
    ret <- data %>% 
        filter(occupancy_date > max_date) %>% 
        mutate(extract_date = Sys.time())

    # Metadata
    # mtd <- str_glue(
    #     "Metadata (Open Data Toronto API):
    #         Last Extract Date: {Sys.time()}
    #         Date Range for New Data Extact: {min(output$occupancy_date)} - {max(output$occupancy_date)}
    #         Max Date in Big Query: {max_date}
    #         New Data Rows: {nrow(output)}
    #         New Data Cols: {ncol(output)}
    #     "
    # )
    
    # Message
    message(str_glue("Raw shelter data extract - Complete!"))
    
    # Return
    return(ret)

}
get_bigquery_upload <-
function(values, 
                                project            = "toronto-shelter-project", 
                                dataset            = "data_raw", 
                                table              = NULL,
                                create_disposition = "CREATE_IF_NEEDED",
                                write_disposition  = "WRITE_APPEND",
                                upload_job         = "shelter") {
    
    # Validate parameters
    stopifnot(
        is.data.frame(values),
        is.character(project), length(project) == 1,
        is.character(dataset), length(dataset) == 1,
        is.character(table), length(table) == 1,
        is.character(create_disposition), length(create_disposition) == 1
    )
    
    # Perform upload
    # job <- bigrquery::insert_upload_job(
    #     values  = values,
    #     project = project,
    #     dataset = dataset,
    #     table   = table,
    #     create_disposition = create_disposition,
    #     write_disposition  = write_disposition
    # )
    
    job <- bigrquery::bq_perform_upload(
        x = bq_table(project = project, dataset = dataset, table = table),
        values = values,
        write_disposition = write_disposition,
        create_disposition = create_disposition,
        quiet = FALSE
    )
    
    # Message
    job_time <- tibble(
        creation_time = as.numeric(job$statistics$creationTime),
        start_time    = as.numeric(job$statistics$startTime)
    ) %>% 
        mutate(creation_time = format(
            as.POSIXct(creation_time / 1000, origin = "1970-01-01"), "%Y-%m-%d %I:%M %p"
        )) %>% 
        mutate(start_time = format(
            as.POSIXct(start_time / 1000, origin = "1970-01-01"), "%Y-%m-%d %I:%M %p"
        ))
    
    # Metadata
    if (upload_job == "shelter") {
        mtd_title = "Metadata (Shelter Occupancy Data Upload):"
    } else if (upload_job == "weather") {
        mtd_title = "Metadata (AccuWeather Forecast Data Upload):"
    }

    mtd <- stringr::str_glue(
            "
            {mtd_title}
                Job Status: {job$status}
                Job ID: {job$jobReference$jobId}
                Job Creation Time: {job_time$creation_time}
                Job Start Time: {job_time$start_time}
            "
        )
    
    message(mtd)
    
    return(mtd)
}
get_bigquery_connection <-
function (project = "toronto-shelter-project",
                                     dataset = "data_raw") {
    
    # Connect to BigQuery
    con <- DBI::dbConnect(
        bigrquery::bigquery(),
        project = project,
        dataset = dataset,
        billing = project  
    )
    
    # Message
    if (!is.null(con)) {
        msg = "BiqQuery connection - Complete"
    } else {
        msg = "No BiqQuery connection established"
    }
    
    message(str_glue(msg))
    
    # Message
    return(con)
}
