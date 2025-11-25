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


##############
##   Header  #
##############


header <- dashboardHeader(title = "Load Forecasting")

##############
##   Sider   #
##############

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro",tabName = "p1", icon = icon("chalkboard")),
    menuItem("Eval",tabName = "p2", icon = icon("ruler"))

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
                       
                       #################################
                       #         Descargar muestra     #
                       #################################
                       
                       h2("Descargar la muestra seleccionada"),
                       br(),
                       actionButton("show1", "Descargar archivo")
  ),
  
  tabItem(tabName = "p2",
          
          
          h1("Evalución de la auditoría.", align = "center"),
          br(),
          br(),
          h3("Seleccionar el archivo a evaluar."),
          fileInput("file2", "Importar datos para la evaluación del muestreo.",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          h3("Seleccionar los parametros para la evaluación de los valores observados y auditados."),
          br(),
          sliderInput("freq20",
                      "Materialidad:",
                      min = 0.01,  max = 0.99, value = 0.05),
          selectInput("method", "Seleccione el método de evaluación:",  
                      list(`Tipo` = list( "poisson", 
                                          "binomial",
                                          "hypergeometric",
                                            "stringer.poisson",
                                          "stringer.binomial", 
                                          "stringer.hypergeometric",
                                             "stringer.meikle",
                                          "stringer.lta", 
                                          "stringer.pvz",
                                             "rohrbach", 
                                          "moment", 
                                          "coxsnell", 
                                          "mpu",
                                             "direct",
                                          "difference", 
                                          "quotient", 
                                          "regression"
                      )
                      )
          ),
          sliderInput("freq21",
                      "Nivel de confianza:",
                      min = 0.01,  max = 0.99, value = 0.95),
          
          uiOutput("var1"),
          uiOutput("var2"),
          br(),
          actionButton("analizar", "Analizar Correlación"),
          br(),
          br(),
          plotOutput("plotCorrelation"),
          br(),
          verbatimTextOutput("eval")
          
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
  
  
  ########################
  #  Datos Reactive      #
  ########################
  
  Muestra <- reactive({
                 
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
    sample
  })
  
  
  #################################
  #         Descargar muestra     #
  #################################
  
  observeEvent(input$show1, {
    
    showModal(modalDialog(
      title = "Descargar los datos ", br(),
      br(),
      downloadButton("download2.1",".csv file"),
      br(),
      br(),
      downloadButton("download2.2",".txt file"),
      footer = modalButton("Close"),
      easyClose = TRUE)
    )
    
  })
  
  output$download2.1 <- downloadHandler(
    
    
    filename = function() {
      paste("Muestra-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Muestra(), file)
    }
  )
  
  output$download2.2 <- downloadHandler(
    
    filename = function() {
      paste("Muestra-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(Muestra(), file)
    }
  )
  
  
  ###########################################
  #                Evaluación               #
  ###########################################
  
  data2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  output$var1 <- renderUI({
    selectInput("select_var1", "Seleccione Variable 1:", names(data2()))
  })
  
  output$var2 <- renderUI({
    selectInput("select_var2", "Seleccione Variable 2:", names(data2()))
  })
  
  observeEvent(input$analizar, {
    output$plotCorrelation <- renderPlot({
      plot(data2()[,input$select_var1], data2()[,input$select_var2], 
           main=paste("Correlación entre", input$select_var1, "y", input$select_var2), 
           xlab=input$select_var1, ylab=input$select_var2)
    })
  })
  
  
  
  
   # Structura #
  
  
  observeEvent(input$analizar, {
  output$eval <- renderPrint({
   
    stage4 <- evaluation(
      materiality = 0.03, 
      method = "stringer",
      conf.level = 0.95, 
      data = data2(),
      values = input$select_var1,
      values.audit = input$select_var2
    )
    
    summary(stage4)
    
     })
  })
  
}

###########
## Run App  
###########

shinyApp(ui, server)



