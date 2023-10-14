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
ui <- dashboardPage(
    dashboardHeader(
        title = "Simple Shiny App",
        # Message Menu ----
        dropdownMenu(type = "messages",
                     messageItem(
                         from = "Sales Dept",
                         message = "Sales are steady this month."
                     ),
                     messageItem(
                         from = "New User",
                         message = "How do I register?",
                         icon = icon("question"),
                         time = "13:45"
                     ),
                     messageItem(
                         from = "Support",
                         message = "The new server is ready.",
                         icon = icon("life-ring"),
                         time = "2014-12-01"
                     )
        )
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Tab 1", tabName = "tab1", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "home",
                h1("Welcome to the Home Page"),
                p("This is the Home tab. You can put your home page content here.")
            ),
            tabItem(
                tabName = "tab1",
                h1("Predictions"),
                hr(),
                sidebarLayout(
                    sidebarPanel(
                        width = 2,
                        br(),
                        pickerInput(
                            inputId  = "location_id",
                            label    = "Location ID",
                            choices  = NULL,
                            multiple = TRUE,
                            selected = NULL
                        )
                    ),
                    
                    mainPanel()
                )
            )
        )
    )
)

server <- function(input, output) {
    # Server logic here
}

shinyApp(ui, server)






# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output) {
    
 
}

shinyApp(ui, server)