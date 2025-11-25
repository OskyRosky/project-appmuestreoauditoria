library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Análisis de distribución de frecuencia"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Cargar archivo CSV",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("variable_select")
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  output$variable_select <- renderUI({
    if (is.null(data())) {
      return(NULL)
    } else {
      selectInput("variable", "Elija una variable:", names(data()))
    }
  })
  
  output$histogram <- renderPlot({
    if (is.null(input$variable)) {
      return(NULL)
    }
    ggplot(data(), aes_string(input$variable)) + geom_histogram(binwidth = 10) + labs(title = paste("Distribución de", input$variable), x = input$variable, y = "Frecuencia")
  })
}

shinyApp(ui = ui, server = server)