


prediction_info_button_UI <- function(id) {
  ns <- NS(id)
  tagList(
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
               p("The table below contains overnight shelter room & bed availability"),
               p("based on multiple features like location id, program id, weather, etc"),
               
               p("Use the", code("toggle"),  "button to toggle user inputs"),
               p(tags$a(img(src = "toggle_button.png", width = "60%", height = "60%"))),
               
               p(""),
               p(""),
               p(""),
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