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
source(file = "modules/analysis/reporting.R")
source(file = "modules/analysis/extract_shelter_data.R")
source(file = "modules/app/ui_server_modules.R")
source(file = "modules/app/prediction_info_button.R")

# *****************************************************************************
# **** ----
# UI ----
# *****************************************************************************
# Define UI for the app
ui <- tagList(
    useShinydashboard(),
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(".box.box-info > .box-header {background-color: ##ecf0f1 !important;}"))),
    tags$head(tags$style(HTML(".box {border-color: #ecf0f1 !important;}"))),
    tags$style(".custom-modal .modal-dialog {width: 100%;}"),
    tags$script(src="https://kit.fontawesome.com/77fcf700e6.js"),
    includeCSS("www/styles.css"),

    
    
    navbarPage(
      theme = shinytheme("flatly"),
        
        tabPanel(
            title = "Home", icon = icon("home"),
            
            fluidPage(
                column(
                    width = 12, offset = 1
                    #p("Home Tab Placeholder")
                )
            )
        ),
        
      # * Preditions Tab ----
        tabPanel(
            title = "Predictions",
            
            # ** Header Fluid Row ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        class = "page-header",
                        h1("Predictions"),
                        h3("Overnight Room & Bed Shelter Occupancy - Next 5 Days"),
                        prediction_info_button_UI("pred_tab_info")
                    )
                )
            ), # ---- End Header Fluid Row ---- #
            
            # ** Well Panel Fluid Row ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    padding = "10px",
                    wellPanel(
                        fluidRow(
                            div(
                                actionButton("toggle", "Toggle Inputs")
                            ),
                            br(),
                            div(
                                id = "inputs",
                                div(
                                    class = "row",
                                    div(
                                        class = "col-md-2",
                                        dateRangeInput(
                                            inputId = "date_info",
                                            label   = "Date",
                                            start   = NULL,
                                            end     = NULL,
                                            min     = NULL,
                                            max     = NULL
                                        )
                                    ),
                                    div(
                                        class = "col-md-2",
                                        pickerInput(
                                            inputId  = "location_info",
                                            label    = "Location Info",
                                            choices  = NULL,
                                            multiple = TRUE,
                                            selected = NULL,
                                            options = list(
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deselect All",
                                                `select-all-text` = "Select All",
                                                `none-selected-text` = "Nothing Selected",
                                                `selected-text-format` = "count > 1"
                                            )
                                        )
                                    ),
                                    div(
                                        class = "col-md-2",
                                        pickerInput(
                                            inputId  = "organization_info",
                                            label    = "Organization Info",
                                            choices  = NULL,
                                            multiple = TRUE,
                                            selected = NULL,
                                            options = list(
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deselect All",
                                                `select-all-text` = "Select All",
                                                `none-selected-text` = "Nothing Selected",
                                                `selected-text-format` = "count > 1"
                                            )
                                        )
                                    ),
                                    div(
                                        class = "col-md-3",
                                        pickerInput(
                                            inputId  = "program_info",
                                            label    = "Program Info",
                                            choices  = NULL,
                                            multiple = TRUE,
                                            selected = NULL,
                                            options = list(
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deselect All",
                                                `select-all-text` = "Select All",
                                                `none-selected-text` = "Nothing Selected",
                                                `selected-text-format` = "count > 1"
                                            )
                                        )
                                    )
                                ),
                                div(
                                    actionButton("apply", "Apply", icon = icon("play"), width = "140px"),
                                    actionButton("reset", "Reset", icon = icon("sync"), width = "140px"),
                                    actionButton("download", "Download Data", icon = icon("download"), width = "140px"),
                                    actionButton("mtd", "Metadata", icon = icon("info-circle"), width = "140px")
                                    
                                )
                            ) %>% shinyjs::hidden()
                        )
                    )
                )
            ), # ---- End Well Panel Fluid Row ---- #
            
            # ** Value Boxes Fluid Row ----
            fluidRow(
              column(
                width = 10, offset = 1,
                fluidRow(
                  column(width = 4, value_box_UI("pred_capacity_bed")),
                  column(width = 4, value_box_UI("pred_occupied_bed")),
                  column(width = 4, value_box_UI("pred_rate_bed")),
                  column(width = 4, value_box_UI("pred_capacity_room")),
                  column(width = 4, value_box_UI("pred_occupied_room")),
                  column(width = 4, value_box_UI("pred_rate_room"))
                )
              )
            ), # ---- End Value Box Fluid Row ---- #
            
            # ** Datatable Fluid Row ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    
                    br(),
                   
                    div(
                        box(
                            width = 12,
                            solidHeader = TRUE,
                            rounded = TRUE,
                            h3("Predictions", tags$span(id = "pred_dt"), icon("info-circle")),
                            status = "info",
                            #dataTableOutput("prediction_dt")
                            DT::dataTableOutput("prediction_dt")
                        ),
                        bsPopover(
                            id = "pred_dt",
                            title = "Predictions",
                            content = "Shelter overninght occupancy for the next 5 days",
                            placement = "left"
                        )
                    )
                )
            ) # ---- End Datatable Fluid Row ---- #
            
        ),
        
        # * Accuracy Tab ----
        tabPanel(
            title = "Accuracy",
            
            # ** Header Panel Fluid Row ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        class = "page-header",
                        h1("Accuracy"),
                        p("Coming Soon..."),
                        actionButton("info", "Get Info", icon("info-circle"))
                    )
                )
            ) # ---- End Header Panel Fluid Row ---- #
            
        )
    )
)







# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output) {
    
    # * Info Button ----
   observeEvent(input[["pred_tab_info-info"]], {
       prediction_info_button_Server("pred_tab_info")
   })
    
    # Metadata
    mtd_list <- reactive({
        read_rds("artifacts/metadata_list.rds")
    })
    
    # AccuWeather
    output$api_mtd <- renderText({
        mtd_list()[[1]]
    })
    
    # Big Query
    output$bq_mtd <- renderText({
        mtd_list()[[2]]
    })
    
    # ---- End Info Button ---- #
    
    # * Load Prediction Data ----
    reporting_tbl <- reactive({
       tbl <- get_reporting_data_from_bq() %>% 
         #reporting_tbl %>% 
           select(-c(pkey)) %>% 
           mutate(loc_info = paste(location_id, ": ", location_name), .before = shelter_id) %>% 
           mutate(org_info = paste(organization_id, ": ", organization_name), .before = shelter_id) %>% 
           mutate(prog_info = paste(program_id, ": ", program_name), .before = shelter_id) %>% 
           mutate(fmt = case_when(
               pred_occupancy_rate_adj <= 0.80 ~ "green",
               pred_occupancy_rate_adj <= 0.90 ~ "orange",
               TRUE                            ~ "red"
           )) 
       
       tbl
       
    })
    
    
    # * Apply Button ----
    predictions_filtered_tbl <- eventReactive(input$apply, valueExpr = {
        #reactive({
        reporting_tbl() %>% 
            arrange(occupancy_date) %>% 
            filter(occupancy_date %>% between(input$date_info[1], input$date_info[2])) %>% 
            filter(loc_info %in% input$location_info) %>% 
            filter(org_info %in% input$organization_info) %>%
            filter(prog_info %in% input$program_info) %>%
            select(occupancy_date, location_id, program_id, shelter_id, sector, 
                   overnight_service_type, capacity_type,pred_capacity_actual, 
                   pred_occupied_adj, pred_available, pred_fully_occupied_adj, 
                   pred_occupancy_rate_adj, fmt) %>% 
            mutate(pred_occupancy_rate_adj = scales::percent(pred_occupancy_rate_adj, accuracy = 0.02)) %>% 
            mutate(capacity_type = case_when(
                str_detect(capacity_type, "Bed") ~ "Bed",
                TRUE                             ~ "Room"
            ))
            #setNames(names(.) %>% str_remove_all("pred_")) 
        # })
    }, ignoreNULL = FALSE)
    
    observeEvent(input$location_info, {
        shinyjs::delay(ms = 100, expr = {
            shinyjs::click(id = "apply")
        })
    }, once = TRUE)
    
    
    
    # * Location Info Picker Update ----
    shiny::observe({
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "location_info",
            choices  = unique(reporting_tbl()$loc_info),
            selected = unique(reporting_tbl()$loc_info)
        )
    })
    
    # * Date Picker Update ----
    shiny::observe({
        updateDateRangeInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "date_info",
            start    = min(reporting_tbl()$occupancy_date),
            end      = max(reporting_tbl()$occupancy_date),
            min      = min(reporting_tbl()$occupancy_date),
            max      = max(reporting_tbl()$occupancy_date)
        )
    })
    
    # * Organization Name Picker Update ----
    shiny::observeEvent(input$location_info, {

        # Organization Name Picker Input
        org_names <- reporting_tbl() %>%
            filter(loc_info %in% input$location_info) %>%
            select(org_info)

        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "organization_info",
            choices  = unique(org_names$org_info),
            selected = unique(org_names$org_info)
        )
    }, ignoreNULL = FALSE)

    # * Program Name Picker Update ----
    shiny::observe({

        # Program Names
        prog_names <- reporting_tbl() %>%
            filter(loc_info %in% input$location_info) %>%
            filter(org_info %in% input$organization_info) %>%
            select(prog_info)

        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "program_info",
            choices  = unique(prog_names$prog_info),
            selected = unique(prog_names$prog_info)
        )
    })
    
    # * Toggle ----
    shinyjs::onclick(id = "toggle", {
        shinyjs::toggle(id = "inputs", anim = TRUE, animType = "slide")
    })
    
    
    # * Prediction Datatable ----
    output$prediction_dt <- renderDT({
        # predictions_filtered_tbl() %>%
        #     DT::datatable(
        #         options = list(
        #             columnDefs = list(
        #                 list(className = "dt-center", targets = c(8:12)),
        #                 list(visible = FALSE, targets = c(13))
        #             )
        #         )
        #     ) %>%
        #     formatStyle(
        #         "pred_occupancy_rate_adj", "fmt",
        #         backgroundColor = styleEqual(
        #             c("green", "orange", "red"), c("#93c47d", "#ffe599", "#ea9999")
        #         )
        #     )
      predictions_filtered_tbl() %>% 
        setNames(names(.) %>% str_remove_all("_adj")) %>% 
        datatable(
          options = list(
            columnDefs = list(
                  list(className = "dt-center", targets = c(8:12)),
                  list(visible = FALSE, targets = c(13))
              )
          )
        ) %>% 
        formatStyle(
                  "pred_occupancy_rate", "fmt",
                  backgroundColor = styleEqual(
                      c("green", "orange", "red"), c("#93c47d", "#ffe599", "#ea9999")
                  )
              )
    
    })

    
    # * Reset Button ----
    observeEvent(eventExpr = input$reset, handlerExpr = {
        
        # location info
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "location_info",
            choices  = unique(reporting_tbl()$loc_info),
            selected = unique(reporting_tbl()$loc_info)
        )
        
        # org info
        org_names <- reporting_tbl() %>% 
            filter(loc_info %in% input$location_info) %>% 
            select(org_info)
        
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "organization_info",
            choices  = unique(org_names$org_info),
            selected = unique(org_names$org_info)
        )
        
        # program info
        prog_names <- reporting_tbl() %>% 
            filter(loc_info %in% input$location_info) %>% 
            filter(org_info %in% input$organization_info) %>% 
            select(prog_info)
        
        updatePickerInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "program_info",
            choices  = unique(prog_names$prog_info),
            selected = unique(prog_names$prog_info)
        )
        
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "apply")
        })
        
        
    })
    
    
    # * Metadata Modal ----
    observeEvent(input$mtd, {
        showModal(
            modalDialog(
                textOutput("api_mtd"), 
                textOutput("bq_mtd"),
                size = "l", easyClose = TRUE, fade = TRUE,
                footer = modalButton("Close (Esc)")
            )
        )
    
    })
    
    # * Value Boxes ----
    value_box_Server(
      id        = "pred_capacity_bed",
      data      = predictions_filtered_tbl,
      value     = reactive("capacity_bed"),
      sub_title = reactive("Predicted Capacity Beds"),
      icon      = "bed"
    )

    value_box_Server(
      id        = "pred_occupied_bed",
      data      = predictions_filtered_tbl,
      value     = reactive("occupied_bed"),
      sub_title = reactive("Predicted Occupied Beds"),
      icon      = "bed"
    )
    
    value_box_Server(
      id        = "pred_rate_bed",
      data      = predictions_filtered_tbl,
      value     = reactive("bed_rate"),
      sub_title = reactive("Predicted Occupancy Rate Beds"),
      icon      = "bed"
    )
    
    value_box_Server(
      id        = "pred_capacity_room",
      data      = predictions_filtered_tbl,
      value     = reactive("capacity_room"),
      sub_title = reactive("Predicted Capacity Rooms"),
      icon      = "person-shelter"
    )
    
    value_box_Server(
      id        = "pred_occupied_room",
      data      = predictions_filtered_tbl,
      value     = reactive("occupied_room"),
      sub_title = reactive("Predicted Occupied Rooms"),
      icon      = "person-shelter"
    )
    
    value_box_Server(
      id        = "pred_rate_room",
      data      = predictions_filtered_tbl,
      value     = reactive("room_rate"),
      sub_title = reactive("Predicted Occupancy Rate Rooms"),
      icon      = "person-shelter"
    )
 
}


# *****************************************************************************
# **** ----
# RUN APP ----
# *****************************************************************************
shinyApp(ui, server)



