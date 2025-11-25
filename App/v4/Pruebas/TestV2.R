library(shiny)

# Create the data frame
df <- data.frame(
  nombre = c("Juan", "Pedro", "María"),
  edad = c(20, 21, 22)
)

# Create the UI
ui <- fluidPage(
  actionButton("myButton", "Click me!"),
  tableOutput("myTable")
)

# Create the server
server <- function(input, output) {
  output$myTable <- renderTable({
    if (input$myButton) {
      df
    } else {
      NULL
    }
  })
}

# Run the app
shinyApp(ui, server)