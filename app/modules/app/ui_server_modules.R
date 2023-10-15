

picker_input_UI <- function(id, label = NULL) {
  ns <- NS(id)
  tagList(
      pickerInput(
          inputId  = ns("location_id"),
          label    = label,
          choices  = NULL,
          selected = NULL,
          multiple = TRUE
      )
  )
}

picker_input_Server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
        
        observe({
            if (input$toggle) {
                updatePickerInput(
                    session,
                    inputId  = "location_id",
                    choices  = unique(data$location_id),
                    selected = unique(data$location_id[1])
                )
            }
        })
     }
   )
}