get_shelter_data <-
function(year = 2023) {

    # info
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
        
    # info check
    if (is.null(info) || length(info) == 0) {
        stop("No API info extracted! Check API info code chunk", call. = FALSE)
    }
    
    # data
    df <- info %>% 
        get_resource() %>% 
        janitor::clean_names() %>% 
        mutate(occupancy_date = lubridate::ymd(occupancy_date))
    
    if (is.null(df) || length(df) == 0) {
        stop("No data extracted! Check data chunk", call. = FALSE)
    }
    
    # max date
    max_date <- max(df$occupancy_date)
    
    # filter
    ret <- df %>% filter(occupancy_date > max_date - 1)
    
    # Metadata
    mtd <- str_glue(
        "Metadata (Open Data Toronto API):
            Rows: {nrow(ret)}
            Cols: {ncol(ret)}
            Date: {unique(ret$occupancy_date)}"
    )
    
    # Message
    message(mtd)
    
    return(list(data = ret, metadata = mtd))
}
get_bigquery_upload <-
function(values, 
                                project            = "toronto-shelter-project", 
                                dataset            = "data_raw", 
                                table              = NULL,
                                create_disposition = "CREATE_IF_NEEDED",
                                write_disposition  = "WRITE_APPEND") {
    
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
        mutate(creation_time = format(
            as.POSIXct(creation_time / 1000, origin = "1970-01-01"), "%Y-%m-%d %I:%M %p"
        )) %>% 
        mutate(start_time = format(
            as.POSIXct(start_time / 1000, origin = "1970-01-01"), "%Y-%m-%d %I:%M %p"
        ))

    mtd <- stringr::str_glue(
            "
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
    
    return(con)
}
