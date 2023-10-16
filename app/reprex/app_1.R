

library(tidyverse)
library(shiny)
library(shinyjs)

# Generate random data
set.seed(123)
num_rows <- 100

location_ids <- sample(1000:9999, 20, replace = TRUE)
org_ids      <- c(rep(1234, 5), rep(3456, 3), rep(4567, 5), rep(5687, 7))
shop_id      <- c(rep(1234, 5), rep(3456, 3), rep(4567, 5), rep(5687, 7))


program_ids <- sample(1000:9999, num_rows, replace = TRUE)
cost <- runif(num_rows, min = 10, max = 100)
sales <- runif(num_rows, min = 50, max = 500)

data_tibble <- tibble(
    location_id = location_ids,
    org_id = org_ids,
    program_id = program_ids,
    cost = cost,
    sales = sales
)

# Define the Shiny app UI
ui <- fluidPage(
    titlePanel("Basic Shiny App"),
    dataTableOutput("data_table")
)

# Define the server logic
server <- function(input, output) {
    output$data_table <- renderDataTable({
        data_tibble
    })
}

