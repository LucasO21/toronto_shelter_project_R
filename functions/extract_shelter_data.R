get_shelter_data <-
function(slice = 1) {
    
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
get_bigquery_upload <-
function(project = "toronto-shelter-project", 
                                dataset = "data_raw", table = NULL, values = NULL,
                                create_disposition = "CREATE_IF_NEEDED",
                                write_disposition = "WRITE_TRUNCATE") {
    
    # Validate parameters
    stopifnot(
        is.data.frame(values),
        is.character(project), length(project) == 1,
        is.character(dataset), length(dataset) == 1,
        is.character(table), length(table) == 1,
        is.character(create_disposition), length(create_disposition) == 1
    )
    
    # Perform upload
    job <- bigrquery::insert_upload_job(
        values  = values,
        project = project,
        dataset = dataset,
        table   = table,
        create_disposition = create_disposition,
        write_disposition  = write_disposition
    )
    
    # Message
    job_time <- tibble(
        creation_time = as.numeric(job$statistics$creationTime),
        start_time    = as.numeric(job$statistics$startTime)
    ) %>% 
        mutate(creation_time = as.POSIXct(
            creation_time / 1000, origin = "1970-01-01", tz = "America/New_York"
        ) + 2*3600) %>% 
        mutate(start_time = as.POSIXct(
            start_time / 1000, origin = "1970-01-01", tz = "America/New_York"
        ) + 2*3600)

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
    
    return(con)
}
