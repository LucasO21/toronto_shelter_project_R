


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
          p("The table below contains overnight shelter room & bed availability"),
          p("based on multiple features like location id, program id, weather, etc"),
          
          p("Use the", code("toggle"),  "button to toggle user inputs"),
          p(tags$a(img(src = "toggle_button.png", width = "60%", height = "60%"))),
          
          p("Change", code("Date"), ",", code("Location Info"), ",", code("Organization Info"), ",", "and"),
          p("and", code("Program Info"), "inputs as needed to filter for a particular day/location."),
          p(tags$a(img(src = "well_inputs.png", width = "100%", height = "120%"))),
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
