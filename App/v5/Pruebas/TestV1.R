############################
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
suppressMessages(library(reactable))
suppressMessages(library(RcppRoll))
suppressMessages(library(sunburstR))
suppressMessages(library(htmltools))
suppressMessages(library(d3r))



##############
##   Header  #
##############


header <- dashboardHeader(title = "Load Forecasting")

##############
##   Sider   #
##############

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Test",tabName = "p1", icon = icon("chalkboard"))
  )
)
##########
## Body  #
##########

body <- dashboardBody( 
  tabItems(    tabItem(tabName = "p1",
                       h1("Introducción", align = "center"),
                       br(),
                       fileInput("file", "Choose a CSV file"),
                   #   actionButton("load", "Load"),
                       tableOutput("data"),
                       tableOutput("dim"),
                       verbatimTextOutput("str"),
                       plotOutput("hist")
                       
                       
                       )
    
  )
  
)

##########
#   Ui   #
##########

ui <- dashboardPage(
  
  
  header,
  sidebar,
  body
)

###########
#  Server #
###########

server <- function(input, output) {
  
  data <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    }
    read.csv(input$file$datapath)
  })
  
  output$data <- renderTable({
    head(data(), 20)
  })
  
  output$dim <- renderTable({
    dim(data())
  })
  
  output$str <- renderPrint({
    str(data())
  })
  
  output$hist <- renderPlot({
    hist(data()[, "Healthy.life.expectancy"])
  })
  
}

shinyApp(ui = ui, server = server)



