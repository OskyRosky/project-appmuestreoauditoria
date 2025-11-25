###########################
#         App Muestreo     #
############################

###############
#  Librerías  # 
###############

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(DT))
suppressMessages(library(plyr))
suppressMessages(library(readr))
suppressMessages(library(janitor))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinydashboardPlus))
suppressMessages(library(highcharter))
suppressMessages(library(formattable))
suppressMessages(library(highcharter))
suppressMessages(library(viridisLite))
suppressMessages(library(stringi))
suppressMessages(library(data.table))
suppressMessages(library(tidyr))
suppressMessages(library(forecast))
suppressMessages(library(kableExtra))
suppressMessages(library(shinyWidgets))
suppressMessages(library(png))
suppressMessages(library(scales))
suppressMessages(library(gt))
suppressMessages(library(ggplot2))
suppressMessages(library(reactable))
suppressMessages(library(RcppRoll))
suppressMessages(library(sunburstR))
suppressMessages(library(htmltools))
suppressMessages(library(d3r))
suppressMessages(library(jfa))
suppressMessages(library(readxl))
suppressMessages(library(dplyr))


ui <- fluidPage(
  fileInput('file', 'Elegir archivo CSV'),
  uiOutput("var1"),
  uiOutput("var2"),
  actionButton("analizar", "Analizar Correlación"),
  plotOutput("plotCorrelation")
)

server <- function(input, output) {
  
  datos <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$var1 <- renderUI({
    selectInput("select_var1", "Seleccione Variable 1:", names(datos()))
  })
  
  output$var2 <- renderUI({
    selectInput("select_var2", "Seleccione Variable 2:", names(datos()))
  })
  
  observeEvent(input$analizar, {
    output$plotCorrelation <- renderPlot({
      plot(datos()[,input$select_var1], datos()[,input$select_var2], 
           main=paste("Correlación entre", input$select_var1, "y", input$select_var2), 
           xlab=input$select_var1, ylab=input$select_var2)
    })
  })
}

shinyApp(ui = ui, server = server)






