get_automl_recipes <-
function(data, prob = TRUE) {
  
  if (prob) {
    formula <- recipe(occupancy_rate ~ ., data = data)
  } else {
    formula <- recipe(occupied ~ ., data = data)
  }
  
  recipe_spec <- formula %>% 
    # update_role(x_id, new_role = "indicator") %>% 
    # step_mutate(capacity_type = case_when(
    #   str_detect(tolower(capacity_type), "bed") ~ "Bed",
    #   TRUE                                      ~ "Room"
    # ) %>% as_factor) %>% 
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
