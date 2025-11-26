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
suppressMessages(library(reactable))
suppressMessages(library(RcppRoll))
suppressMessages(library(sunburstR))
suppressMessages(library(htmltools))
suppressMessages(library(d3r))
suppressMessages(library(jfa))
suppressMessages(library(readxl))
suppressMessages(library(dplyr))


##############
##   Header  #
##############


header <- dashboardHeader(title = "Load Forecasting")

##############
##   Sider   #
##############

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro",tabName = "p1", icon = icon("chalkboard"))

  )
)

##########
## Body  #
##########

body <- dashboardBody( 
  tabItems(    tabItem(tabName = "p1",
                       
                       
                       h1("Importar de los datos", align = "center"),
                       br(),
                       fileInput("file1", "Importar datos del muestreo",
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       br(),
                       br(),
                       uiOutput("variable_select"),
                       plotOutput("histogram"),
                       br(),
                       br(),
                       h1("Muestreo: tamaño y selección", align = "center"),
                       br(),
                       br(),
                       h2("Cálculo de tamaño de muestra"),
                       br(),
                       sliderInput("freq1",
                                   "Materialidad:",
                                   min = 0.01,  max = 0.99, value = 0.05),
                       sliderInput("freq2",
                                   "Esperado:",
                                   min = 0.01,  max = 0.99, value = 0.01), 
                       selectInput("distri", "Seleccione el nivel:",  
                                   list(`Tipo` = list("poisson",
                                                      "binomial", 
                                                      "hypergeometric"
                                   )
                                   )
                       ),
                       sliderInput("freq3",
                                   "Nivel de confianza:",
                                   min = 0.01,  max = 0.99, value = 0.95),
                       actionButton("update", "Calcular"),
                       hr(),
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           reactableOutput("SampleSize")  
                         )
                       ),
                       br(),
                       br(),
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           reactableOutput("sample")  
                         )
                       ),
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
  
  ####################################################
  #                      IMPORTAR                    #
  ####################################################
  
  ##############################
  #          Importar Datos    #
  ##############################
  
  # Data la volvemos un objeto reactivo 
  #  data ---> data()
  
  
  
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  
  # Histograma de una variable #
  
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
  
  ####################################################
  #                     Muestra                      #
  ####################################################
  
  #################################
  #    Cálculo tamaño muestra     #
  #################################
  
  output$SampleSize <- renderReactable({
    
    if (input$update) {
      
      stage1 <- planning(materiality = input$freq1, 
                         expected = input$freq2,
                         likelihood = input$distri, 
                         conf.level = input$freq3
      )
      
      sample_size <- data.frame(`Muestra` = stage1$n)
      reactable(sample_size)
      
    } else {
      NULL
    }
    
  })
  
  #################################
  #    Visualización de la tabla  #
  #################################
  
  
  output$sample  <- renderReactable({

    if (input$update) {
    
    stage1 <- planning(materiality = input$freq1, 
                       expected = input$freq2,
                       likelihood = input$distri, 
                       conf.level = input$freq3
    )
    
    stage2 <- selection(
      data = data(), 
      size = stage1,      #### Stage1 previous defined 
      units = "values", 
      values = input$variable,   #### Column from  data
      method = "random", start = 2
    )
    
    sample <- stage2[["sample"]]
    
    reactable(sample)
    
    } else {
      NULL
    }
    
    
    
  })
  

  
}

###########
## Run App  
###########

shinyApp(ui, server)



