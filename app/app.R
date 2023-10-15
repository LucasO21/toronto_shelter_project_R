# BIG QUERY APP ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(shinyBS)
library(htmlwidgets)

# * Source ----
source(file = "modules/app/ui_server_modules.R")
source(file = "modules/analysis/reporting.R")
source(file = "modules/analysis/extract_shelter_data.R")

# *****************************************************************************
# **** ----
# UI ----
# *****************************************************************************
# Define UI for the app
ui <- tagList(
    useShinydashboard(),
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(".box.box-info > .box-header {background-color: #95a5a6 !important;}"))),
    tags$head(tags$style(HTML(".box {border-color: #95a5a6 !important;}"))),
    
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
                                                inputId  = "location_info",
                                                label    = "Location",
                                                choices  = NULL,
                                                multiple = TRUE,
                                                selected = NULL
                                            )
                                            #uiOutput("location_id_picker_input")
                                        
                                        ),
                                        div(
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId  = "organization_name",
                                                label    = "Organization Name",
                                                choices  = NULL,
                                                multiple = TRUE,
                                                selected = NULL
                                            )
                                            #uiOutput("organization_id_picker_input")
                                        ),
                                        div(
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId  = "program_name",
                                                label    = "Program Name",
                                                choices  = NULL,
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
                        width = 12, offset = 1,
                        div(
                            h3("Selected Location Info"),
                            textOutput("location_info")
                            
                        ),
                       
                        div(
                            box(
                                width = 10,
                                solidHeader = TRUE,
                                rounded = TRUE,
                                h3("Predictions", tags$span(id = "pred_dt"), icon("info-circle")),
                                status = "info",
                                dataTableOutput("prediction_dt")
                            ),
                            bsPopover(
                                id = "pred_dt",
                                title = "Predictions",
                                content = "Shelter overninght occupancy for the next 5 days",
                                placement = "left"
                            )
                        )
                    )
                ),
                p("Test Area"),
                tableOutput("reporting")
            )
        )
    )
)







# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output) {
    
    # Metadata
    mtd_list <- reactive({
        read_rds("artifacts/metadata_list.rds")
    })
    
    output$api_mtd <- renderText({
        mtd_list()[[1]]
    })
    
    output$bq_mtd <- renderText({
        mtd_list()[[2]]
    })
    
    # * Load Prediction Data ----
    reporting_tbl <- reactive({
       tbl <- get_reporting_data_from_bq() %>% 
           select(-c(x_id, pkey)) %>% 
           mutate(location_info = paste(location_id, ": ", location_name), .before = shelter_id)
       
       tbl
    })
    
    # * Location Info Picker Update ----
    shiny::observe({
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "location_info",
            choices  = unique(reporting_tbl()$location_info),
            selected = unique(reporting_tbl()$location_info[1])
        )
    })

    
    # * Organization Name Picker Update ----
    shiny::observeEvent(input$location_info, {
        
        # Organization Name Picker Input
        org_names <- reporting_tbl() %>% 
            filter(location_info %in% input$location_info) %>% 
            select(organization_name)
        
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "organization_name",
            choices  = unique(org_names$organization_name),
            selected = unique(org_names$organization_name)[1]
        )
    })
    
    # * Program Name Picker Update ----
    shiny::observe({#input$location_info, {
        
        # Program Names
        prog_names <- reporting_tbl() %>% 
            filter(location_info %in% input$location_info) %>% 
            filter(organization_name %in% input$organization_name) %>% 
            select(program_name)
        
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "program_name",
            choices  = unique(prog_names$program_name),
            selected = unique(prog_names$program_name[1])
        )
    })
    
    # * Prediction Datatable ----
    output$prediction_dt <- renderDataTable({
        reporting_tbl() %>% 
            filter(location_info %in% input$location_info) %>% 
            filter(organization_name %in% input$organization_name) %>% 
            filter(program_name %in% input$program_name) %>% 
            select(occupancy_date, program_model, sector, overnight_service_type,
                   capacity_type,pred_capacity_actual, pred_occupied_adj, 
                   pred_available, pred_fully_occupied_adj, pred_occupancy_rate_adj) %>% 
            mutate(pred_occupancy_rate_adj = scales::percent(pred_occupancy_rate_adj, accuracy = 0.02)) %>% 
            mutate(capacity_type = case_when(
                str_detect(capacity_type, "Bed") ~ "Bed",
                TRUE                             ~ "Room"
            )) %>% 
            datatable(
                options = list(
                    columnDefs = list(
                        list(className = "dt-center", targets = c(6:10))
                        #list(width = "200", targets = 3)
                    )
                )
            )
    })
    
    # * Toggle ----
    shinyjs::onclick(id = "toggle", {
        shinyjs::toggle(id = "inputs", anim = TRUE, animType = "slide")
    })
    
    # * Apply Button ----
 
}

shinyApp(ui, server)


