# BIG QUERY APP ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)



# *****************************************************************************
# **** ----
# UI ----
# *****************************************************************************
# Define UI for the app
ui <- tagList(
    useShinydashboard(),
    shinyjs::useShinyjs(),
    
    navbarPage(
        
        tabPanel(
            title = "Home", icon = icon("home"),
            
            fluidPage(
                column(
                    width = 12, offset = 1,
                    p("Home Tab Placeholder")
                )
            )
        ),
        
        tabPanel(
            title = "Predictions",
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        class = "page-header",
                        h1("Predictions"),
                        actionButton("info", "Get Info", icon("info-circle"))
                    )
                ),
                fluidRow(
                    column(
                        width = 10, offset = 1,
                        wellPanel(
                            fluidRow(
                                div(
                                    actionButton("toggle", "Toggle Inputs"),
                                ),
                                br(),
                                div(
                                    id = "inputs",
                                    div(
                                        class = "row",
                                        div(
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId = "location",
                                                label = "Location ID",
                                                choices = c("Location 1", "Location 2", "Location 3"),
                                                multiple = TRUE,
                                                selected = NULL
                                            )
                                        ),
                                        div(
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId = "org",
                                                label = "Org ID",
                                                choices = c("Org 1", "Org 2", "Org 3"),
                                                multiple = TRUE,
                                                selected = NULL
                                            )
                                        ),
                                        div(
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId = "program",
                                                label = "Program ID",
                                                choices = c("Org 1", "Org 2", "Org 3"),
                                                multiple = TRUE,
                                                selected = NULL
                                            )
                                        ),
                                        div(
                                            class = "col-md-6",
                                            p(strong("ETL Metadata")),
                                            textOutput("api_mtd"),
                                            textOutput("bq_mtd")
                                        )
                                    ),
                                    div(
                                        actionButton("apply", "Apply", icon = icon("play")),
                                        actionButton("reset", "Reset", icon = icon("sync")),
                                        
                                    )
                                ) %>% shinyjs::hidden()
                            )
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 10, offset = 1,
                        p("placeholder")
                    )
                )
            )
        )
    )
)







# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output) {
    
    mtd_list <- reactive({
        read_rds("artifacts/metadata_list.rds")
    })
    
    output$api_mtd <- renderText({
        mtd_list()[[1]]
    })
    
    output$bq_mtd <- renderText({
        mtd_list()[[2]]
    })
    
    # * Toggle ----
    shinyjs::onclick(id = "toggle", {
        shinyjs::toggle(id = "inputs", anim = TRUE, animType = "slide")
    })
 
}

shinyApp(ui, server)


