


prediction_info_button_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style("
    .custom-modal .modal-dialog {
      width: 150%;
    }
  "),
      actionButton(ns("info"), "Get Info", icon("info-circle"))
  )
}

prediction_info_button_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      showModal(
        modalDialog(
          title = "Prediction Information",
          class = "custom-modal",
          p("The table below contains overnight shelter room & bed availability
            based on multiple features like location_id, program_id, weather, etc."),
          p("Use the", code("toggle"),  "button to toggle user inputs"),
          p(tags$a(img(src = "toggle_button.png", width = "10%", height = "10%"))),
          hr(),
          p("Change", code("Date"), ",", code("Location Info"), ",", code("Organization Info"), ",", 
            code("Program Info"), "inputs as needed to filter for a particular day/location."),
          p(tags$a(img(src = "well_inputs.png", width = "100%", height = "150%"))),
          hr(),
          p("Hit", code("Apply"), "button to see changes in the table below. Hit", code("Reset"), "to reset entire table."),
          hr(),
          p(strong("pred_capacity_actual"), "is an estimate of", strong("Actual"), "room or bed capacity"),
          p(strong("pred_occupied"), "is the predicted number of occupied rooms or beds for a given day"),
          p(strong("Note:"), "in the shelter occupancy dataset, there are two measures of capacity;", strong("Funding"), "
            capacity reports the number of beds or rooms that a program is intended to provide.", strong("Actual"), "capacity 
            reports the number of beds or rooms in service and showing as available for occupancy 
            in the shelter management information system at the time of reporting. There are 
            a number of reasons why beds or rooms may be temporarily out of service, including 
            maintenance, repairs, renovations, etc. Thus for making future predictions, a 7 
            day average of", strong("Actual"), "capacity was used to estimate", strong("Actual"), "capacity. Also if the 
            model’s prediction of overnight shelter occupancy for beds or rooms (pred_occupied) 
            exceeded the actual capacity (pred_capacity_actual), the pred_occupied value was adjusted down to the pred_capacity_actual
            value. For example, if the pred_capacity_actual for beds in a particular location 
            was estimated to be 70, and the model predicted occupied beds for this location 
            on a given night to be 73, this predicted value of 73 was adjusted down to 70 
          (to match the actual capacity), and thus resulting in predicted occupancy rate of 100%."),
          size = "l", easyClose = TRUE, fade = FALSE,
          footer = tagList(
            modalButton("Close (Esc)")
          )
        )
      )
      
    }
  )
}
