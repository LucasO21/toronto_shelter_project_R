library(shiny)

# Define UI
ui <- fluidPage(
    # Application title
    titlePanel("Histogram of mtcars Variables"),
    
    # Sidebar with a dropdown menu input
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "var", 
                label = "Choose a variable", 
                choices = names(mtcars),
                selected = "mpg"
            )
        ),
        
        # Show a plot of the selected variable
        mainPanel(
            plotOutput("histogram")
        )
    )
)

# Server logic
server <- function(input, output) {
    output$histogram <- renderPlot({
        # Use the input$var to access the variable selected by the user
        data <- mtcars[[input$var]]
        
        # Draw the histogram with the selected variable
        hist(data, main = paste("Histogram of", input$var), xlab = input$var)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
