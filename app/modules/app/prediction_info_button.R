


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
          p(""),
          p(""),
          size = "l", easyClose = TRUE, fade = FALSE,
          footer = tagList(
            modalButton("Close (Esc)")
          )
        )
      )
      
    }
  )
}
