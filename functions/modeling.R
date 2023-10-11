get_features_data_from_bigquery <-
function(year = 2023) {
  
  con <- get_bigquery_connection(dataset = "data_features")
  
  tables <- DBI::dbListTables(con)
  
  shelter_tbl <- dplyr::tbl(con, str_glue("feature_shelter_{year}")) %>% 
    collect() %>% 
    mutate(occupancy_date = lubridate::ymd(occupancy_date)) %>% 
    mutate(location_address = str_trim(location_address, side = c("both"))) %>% 
    mutate(location_city = str_trim(location_city, side = c("both")))
  
  weather_tbl <- dplyr::tbl(con, str_glue("feature_weather_{year}")) %>% 
    collect() %>% 
    mutate(date = lubridate::ymd(date))
  
  # message
  message(
    str_glue(
      "
      Shelter Data Info:
        nrow: {nrow(shelter_tbl)}
        ncol: {ncol(shelter_tbl)}
        min date: {min(shelter_tbl$occupancy_date)}
        max date: {max(shelter_tbl$occupancy_date)}
      ====================================================
      Weather Data Info:
        nrow: {nrow(weather_tbl)}
        ncol: {ncol(weather_tbl)}
        min date: {min(weather_tbl$date)}
        max date: {max(weather_tbl$date)}
      "
    )
  )
  
  # return 
  return(list(shelter_tbl, weather_tbl))
  
}
get_weather_data_prepped <-
function(x) {
  
  # diff dates
  shelter_dates <- unique(x[[1]]$occupancy_date)
  weather_dates <- unique(x[[2]]$date)
  diff_dates    <- shelter_dates[!shelter_dates %in% weather_dates]
  
  # tibble of diff dates
  weather_avg <- x[[2]] %>% 
    arrange(date) %>% 
    tail(6) %>% 
    bind_rows(
      tibble(
        stn  = "712650",
        date = c(diff_dates)
      )
    ) 
  
  # get count na values
  na_length <- weather_avg %>% filter(is.na(temp_min)) %>% pull(temp_min) %>% length() + 1
  
 # rolling avg for na values
 weather_avg <- weather_avg %>%  
    mutate(
      across(
        starts_with("temp_"),
        list(
          filled = ~ifelse(
            is.na(.),
            slide_dbl(., ~mean(.x, na.rm = TRUE), .before = na_length  - 1, .complete = TRUE),
            .
          )
        ),
        .names = "{col}_{fn}"
      )
    ) %>% 
    select(-c(temp_min, temp_max, temp_avg)) %>% 
    setNames(names(.) %>% str_remove_all("_filled")) %>% 
    filter(date %in% diff_dates)
 
 # append rolling avg weather
 weather_tbl <- x[[2]] %>% bind_rows(weather_avg)
    
  
  # return
  return(weather_tbl)
  
}
get_modeling_features <-
function(data) {
  
  ret <- data %>% 
    select(
      x_id, occupancy_date, organization_id, shelter_id, location_id, program_id,
      sector, program_model, overnight_service_type, program_area, capacity_type, 
      occupied, capacity_actual, occupancy_rate
    ) %>% 
    mutate(occupancy_rate = ifelse(occupancy_rate == 100, "Yes", "No") %>% as.factor()) %>% 
    mutate_if(is_character, as_factor) %>% 
    mutate(organization_id = as.factor(organization_id)) %>% 
    mutate(shelter_id = as_factor(shelter_id)) %>% 
    mutate(location_id = as_factor(location_id)) %>% 
    mutate(program_id = as_factor(program_id)) 
  
  return(ret)
  
}
get_automl_recipes <-
function(data, prob = TRUE) {
  
  if (prob) {
    formula <- recipe(occupancy_rate ~ ., data = data)
  } else {
    formula <- recipe(occupied ~ ., data = data)
  }
  
  recipe_spec <- formula %>% 
    update_role(x_id, new_role = "indicator") %>% 
    step_mutate(capacity_type = case_when(
      str_detect(tolower(capacity_type), "bed") ~ "Bed",
      TRUE                                      ~ "Room"
    ) %>% as_factor) %>% 
    step_novel(all_nominal(), -all_outcomes()) %>% 
    step_zv(all_predictors()) %>% 
    step_timeseries_signature(occupancy_date) %>% 
    step_rm(matches(".xts|.lbl|.iso|index.num|am.pm|hour|minute|second|hour12")) %>% 
    step_rm(occupancy_date, capacity_actual)
  
  if (prob) {
    recipe_spec <- recipe_spec %>% 
      step_rm(occupied)
  } else {
    recipe_spec <- recipe_spec %>% 
      step_rm(occupancy_rate)
  }
  
  # recipe_spec <- recipe_spec %>% prep()
  
  return(recipe_spec)
  
  
}
get_automl_models <-
function(recipe, train_data, test_data, mrspm = 300, 
                              seed = 123, prob = TRUE) {
  
  # data processing
  train_prep_tbl <- recipe %>% prep() %>% bake(new_data = train_data)
  test_prep_tbl  <- recipe %>% prep() %>% bake(new_data = test_data)
  
  # target / predictor setup
  if (prob) {
    y <- "occupancy_rate"
    x <- setdiff(names(train_prep_tbl), y)
  } else {
    y <- "occupied"
    x <- setdiff(names(train_prep_tbl), y)
  }
  
  # data as h2o objects
  train_h2o <- as.h2o(train_prep_tbl)
  test_h2o  <- as.h2o(test_prep_tbl)
  
  # h2o automl models
  automl_models_h2o <- h2o.automl(
    x                          = x,
    y                          = y,
    training_frame             = train_h2o,
    validation_frame           = test_h2o,
    max_runtime_secs_per_model = mrspm,
    nfolds                     = 5,
    max_models                 = 10,
    exclude_algos              = c("DeepLearning"),
    seed                       = seed
  )
  
  return(
    list(
      h2o_automl_models = automl_models_h2o,
      h2o_train         = train_h2o,
      h2o_test          = test_h2o,
      train_raw         = train_prep_tbl,
      test_raw          = test_prep_tbl
    )
  )
  
}
