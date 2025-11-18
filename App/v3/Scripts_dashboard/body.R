###############################################################
# 📄 body.R — Contenido principal del dashboard (UI)
# -------------------------------------------------------------
# Cambios clave:
#  • Agregada franja de aviso cuando APP_HEAVY ≠ TRUE (usa output.showDownloads)  # NEW
#  • Los botones de descarga de reportes DOCX están dentro de conditionalPanel     # NEW
###############################################################

.ul_style <- "list-style-type: disc; padding-left: 20px;"

body <- shinydashboard::dashboardBody(

  #################################################################
  #                       AVISO GLOBAL (NEW)                      #
  #################################################################
  conditionalPanel(
    condition = "!output.showDownloads",  # NEW
    tags$div(
      style = "background:#fff3cd;border:1px solid #ffeeba;padding:12px;margin:10px 0;border-radius:6px;",
      tags$b("Aviso: "),
      "Los reportes .docx están deshabilitados porque la app no se está ejecutando en modo pesado ",
      tags$code("APP_HEAVY=TRUE"),
      ". Habilítalo e instala ",
      tags$code("officer, flextable"),
      " para activar las descargas de reportes."
    )
  ),

  #################################################################
  #                       CONTENEDOR DE TABS                      #
  #################################################################
  shinydashboard::tabItems(

    #################################################################
    #                           PÁG. p1                             #
    #                        📘 PRESENTACIÓN                        #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p1",

      h1("Guía de usuario para la aplicación de análisis de muestras en unidades monetarias.", align = "center"),
      br(),

      # --- Introducción ---
      h2("Introducción", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Bienvenidos(as) a la aplicación especializada en el análisis de muestras para unidades monetarias.", align = "left"),
      br(),
      h4("Esta herramienta interactiva ha sido diseñada para facilitar el proceso de descripción, de muestreo y de evaluación de una muestra de unidades monetaria.", align = "left"),
      br(),

      # --- Inicie utilizando la App ---
      h2("¡Inicie utilizando la App!", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Para comenzar, en cada sección de la barra lateral izquierda, deberá:", align = "left"),
      br(),
      h4("Navegar", align = "left", style = "font-weight: bold"),
      h4("Navegue entre los diferentes módulos utilizando las pestañas dispuestas en la interfaz del usuario."),
      h4("Cargar Datos", align = "left", style = "font-weight: bold"),
      h4("Utilice el botón gris en las apartados de Cargar  Datos para trabajar su conjunto de datos.", align = "left"),
      h4("Analizar", align = "left", style = "font-weight: bold"),
      h4("Siga las instrucciones específicas en cada sección para realizar el análisis requerido."),
      br(),

      # --- Estructura de la aplicación ---
      h2("Estructura de la Aplicación", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("La aplicación se divide en tres módulos, cada uno enfocado en un aspecto crítico del muestreo en unidades monetarias:", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Análisis Descriptivo", align = "left")),
        tags$li(h4("Proceso de Muestreo",  align = "left")),
        tags$li(h4("Evaluación de la Muestra", align = "left"))
      ),
      br(),
      h4("A continuación, exploramos en detalle cada uno de estos módulos.", align = "left"),
      br(),

      # --- Análisis descriptivo (resumen) ---
      h2("Análisis Descriptivo", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Antes de abordar el proceso de muestreo (tamaño de muestra y selección de las unidades), es esencial comprender el conjunto de datos con el que se trabajará. En el módulo de Análisis Descriptivo, los usuarios podrán:", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Analizar las principales estadísticas descriptivas de la variable de interés.", align = "left")),
        tags$li(h4("Analizar la distribución de la variable de interés.", align = "left")),
        tags$li(h4("Según el análisis de la distribución de la variable de interés, tener una mejor perspectiva del ajuste de la función de distribución.", align = "left"))
      ),
      br(),

      # --- Proceso de muestreo (resumen) ---
      h2("Proceso de Muestreo (MUM y LES)", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Tras un entendimiento del conjunto de datos, se procede con la etapa del Muestreo. Esta etapa se conforma por:", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Determinar el tamaño de muestra.", align = "left")),
        tags$li(h4("Visualizar la selección de los casos (filas) según la determinación del tamaño de muestra.", align = "left")),
        tags$li(h4("Comparar las distribuciones entre los datos orginales y los obtenidos por la muestra.", align = "left")),
        tags$li(h4("Descargar los datos seleccionados en el proceso de muestreo (obtenidos por la muestra).", align = "left"))
      ),
      br(),

      # --- Muestreo por Atributos (resumen) ---
      h2("Muestreo por Atributos", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Se explica cómo obtener una muestra a partir de una variable de atributo. Esta etapa se conforma por:", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Determinar el tamaño de muestra.", align = "left")),
        tags$li(h4("Visualizar la selección de los casos (filas) según la determinación del tamaño de muestra.", align = "left")),
        tags$li(h4("Comparar los porcentajes de las categorías entre los datos orginales y los obtenidos por la muestra.", align = "left")),
        tags$li(h4("Descargar los datos seleccionados en el proceso de muestreo (obtenidos por la muestra).", align = "left"))
      ),
      br(),

      # --- Evaluación (resumen) ---
      h2("Evaluación de la Muestra", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("El último módulo es para obtener un contraste empírico referente al proceso de la auditoría de la muestra obtenida con anterioridad.", align = "left"),
      h4("El foco está en comparar los valores observados (selecionados por la muestra), contra los valores auditados (obtenidos o revisados en el proceso de auditoría).", align = "left"),
      h4("En esta sección de contraste empírico o comparación de datos:", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Se describen y comparan los resultados de una muestra antes seleccionados a nivel visual y los valores que presentan diferencias.", align = "left")),
        tags$li(h4("Se presentan indicadores de riesgo según la comparación entre los valores observados y auditados.", align = "left")),
        tags$li(h4("Se expone la selección de criterios o umbrales máximos tolerables en la evaluación de la muestra.", align = "left"))
      ),

      # --- Reportes ---
      h2("Reportes de análisis", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Al final de cada sección se presenta un 'Descargar Reporte', lo cual permite obtener los resultados del análisis en un formato más adecuado.", align = "left"),
      h4("Cada una de las secciones de análisis puede ser descargada en formato .docx.", align = "left"),
      br(),

      # --- Carga de datos (resumen) ---
      h2("Sobre la carga de datos", align = "left",
         style = "font-weight: bold; text-decoration: underline;"),
      br(),
      h4("Para cada uno de los módulos anteriores, deberá cargar un archivo de datos. La aplicación admite múltiples formatos incluyendo .xlsx, .txt y .csv. Tenga en cuenta que:", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Cada archivo cargado debe contener una sola tabla.", align = "left", style = "font-weight: bold")),
        tags$li(h4("Los datos deben estar limpios, listos para ser analizados.", align = "left", style = "font-weight: bold")),
        tags$li(h4("El peso máximo permitido por archivo es de 100 megabytes, asegurando así la fluidez y eficiencia de la aplicación.", align = "left", style = "font-weight: bold"))
      )
    ),

    #################################################################
    #                           PÁG. p2                             #
    #                    📊 ANÁLISIS DESCRIPTIVO                    #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p2",

      h1("Análisis Descriptivos", align = "center"),
      br(),

      h2("En esta sección:", align = "left"),
      br(),
      h4("Se analiza de forma descriptiva el conjunto de datos.", align = "left"),
      br(),
      h4("Cargando los datos, usted podrá:"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Conocer las principales estadísticas descriptivas de la variable seleccionada.", align = "left")),
        tags$li(h4("Visualizar la distribución de la variable seleccionada (densidad).", align = "left")),
        tags$li(h4("Comparar la distribución con Poisson o Binomial.", align = "left")),
        tags$li(h4("Descargar los resultados en formato '.docx'.", align = "left"))
      ),
      br(),
      h4("Esto guiará la selección de distribución para el tamaño de muestra en la sección siguiente."),
      br(),

      # --- Carga de datos ---
      h3("Cargar datos", align = "left"),
      fileInput(
        "file1", "Importar datos",
        accept = c(
          ".csv", ".txt", ".xlsx",
          "text/csv", "text/plain", "text/tab-separated-values",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      ),
      uiOutput("variable_select_1"),
      h4("IMPORTANTE: Debe seleccionar variables numéricas.", align = "left", style = "font-weight: bold"),
      uiOutput("negativesAlert_1"),
      br(),

      # --- Lanzar análisis ---
      actionButton("start_analysis", "Iniciar Análisis Descriptivos", class = "btn-primary"),
      uiOutput("analysis_output"),
      br(),

      # --- Estadísticas descriptivas ---
      h3("Estadísticas descriptivas", align = "left"),
      br(),
      h4("Se presentan las principales para la variable numérica seleccionada."),
      reactableOutput("stats"),
      br(),

      # --- Distribución ---
      h3("Análisis de distribuciones", align = "left"),
      br(),
      h4("Densidad de la variable numérica seleccionada."),
      br(),
      highchartOutput("histogram1"),

      # --- Comparación de ajustes ---
      h3("Comparación de Ajuste de Distribuciones", align = "left"),
      br(),
      h4("Guía visual para elegir la distribución."),
      fluidRow(
        shinydashboard::box(
          title = "Comparación de distribuciones",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          fluidRow(
            column(width = 6, plotOutput("binomialPlot")),
            column(width = 6, plotOutput("poissonPlot"))
          )
        )
      ),
      h4("Si hay valores muy extremos, Poisson puede ser más adecuado; si están más compactos, Binomial."),
      br(),

      # --- Reporte ---
      h3("Descargar Reporte", align = "left"),
      conditionalPanel(                                           # NEW
        condition = "output.showDownloads",                       # NEW
        downloadButton("downloadReport1", "Descargar Reporte Análisis Descriptivo")
      )
    ),

    #################################################################
    #                           PÁG. p3                             #
    #                         🧮 MUESTREO MUM                       #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p3",

      h1("Muestreo", align = "center"),
      br(),

      h2("En esta sección:", align = "left"),
      br(),
      h4("Se lleva a cabo el proceso de muestreo: tamaño y selección de las unidades.", align = "left"),
      br(),
      h4("Cargando los datos, usted podrá:"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Calcular el tamaño de muestra.", align = "left")),
        tags$li(h4("Visualizar las unidades seleccionadas.", align = "left")),
        tags$li(h4("Comparar los datos cargados vs los datos de la muestra.", align = "left")),
        tags$li(h4("Descargar la muestra en .csv, .txt o .xlsx.", align = "left")),
        tags$li(h4("Descargar los resultados en '.docx'.", align = "left"))
      ),
      br(),

      # --- Carga de datos ---
      h3("Cargar datos", align = "left"),
      br(),
      fileInput(
        "file2", "Importar datos del muestreo",
        accept = c(
          ".csv", ".txt", ".xlsx",
          "text/csv", "text/plain", "text/tab-separated-values",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      ),
      br(),
      uiOutput("variable_select_MUM"),
      h4("IMPORTANTE: Debe seleccionar variables numéricas.", align = "left", style = "font-weight: bold"),
      uiOutput("negativesAlertMuestreoMUM"),
      br(),

      # --- Tamaño y selección ---
      h2("Muestreo: tamaño y selección", align = "left"),
      br(),
      h4("Primero tamaño (tolerable, esperado, confianza) y luego selección (PPT).", align = "left"),
      br(),

      h3("Cálculo de tamaño de muestra"),
      br(),
      h4("Guía rápida sobre parámetros:", align = "left"),
      h4("Margen de Tolerancia (Tolerable)", align = "left", style = "font-weight: bold"),
      h4("Error Esperado (Esperado)", align = "left", style = "font-weight: bold"),
      h4("Nivel de Confianza", align = "left", style = "font-weight: bold"),
      br(),

      h3("Tabla de sugerencia para determinar el tamaño de la muestra.", align = "left"),
      br(),
      h4("El tamaño depende de la capacidad operativa y características de la auditoría.", align = "left"),
      br(),

      fluidRow(
        shinydashboard::box(
          title = "Tabla de Datos",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          div(style = "height: 180px;",
              reactableOutput("SugerenciasTamaño_MUM"))
        )
      ),

      h4("Ajuste los controles y presione 'Análisis del muestreo'.", align = "left"),
      br(),

      sliderInput("freq1_MUM", "Tolerable:", min = 0.01, max = 0.99, value = 0.05),
      sliderInput("freq2_MUM", "Esperado:",  min = 0.01, max = 0.99, value = 0.01),
      h6("Importante: el 'Tolerable' debe ser superior al 'Esperado'."),
      selectInput("distri_1", "Seleccione el nivel:",
                  list(`Tipo` = list("poisson", "binomial"))),
      sliderInput("freq3_MUM", "Nivel de confianza:", min = 0.01, max = 0.99, value = 0.95),

      conditionalPanel(
        condition = "(!output.hasNegatives_MUM) && (input.freq2_MUM < input.freq1_MUM)",
        actionButton("update_MUM", "Análisis del muestreo.", class = "btn-primary")
      ),
      br(), br(),

      # --- Tamaño de la muestra ---
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, width = 12,
          reactableOutput("SampleSize_MUM")
        )
      ),
      br(), br(),

      # --- Semilla ---
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, width = 12,
          reactableOutput("seedvalue_MUM")
        )
      ),
      br(), br(),

      # --- Seleccionados ---
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, width = 12,
          reactableOutput("sample_MUM")
        )
      ),
      br(),

      # --- Comparación de distribuciones ---
      h3("Comparación de datos cargados vs muestra seleccionada"),
      br(),
      fluidRow(
        shinydashboard::box(
          title = "Comparación de distribuciones entre datos cargados y la muestra",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          highchartOutput("comp_dist_MUM")
        )
      ),

      # --- Descargas ---
      h3("Descargar la muestra seleccionada"),
      br(),
      actionButton("show1_MUM", "Descargar archivo"),
      br(),
      h3("Descargar Reporte", align = "left"),
      conditionalPanel(                                           # NEW
        condition = "output.showDownloads",                       # NEW
        downloadButton("downloadReport2", "Descargar Reporte Muestreo MUM")
      )
    ),

    #################################################################
    #                           PÁG. p4                             #
    #                         🧮 MUESTREO LES                       #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p4",

      h1("Muestreo LES", align = "center"),
      br(),

      h2("En esta sección:", align = "left"),
      br(),
      h4("Muestreo por LES: tamaño y selección.", align = "left"),
      br(),
      h4("Cargando los datos, usted podrá:"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Calcular el tamaño de muestra.", align = "left")),
        tags$li(h4("Visualizar las unidades seleccionadas.", align = "left")),
        tags$li(h4("Comparar datos cargados vs muestra.", align = "left")),
        tags$li(h4("Descargar la muestra.", align = "left")),
        tags$li(h4("Descargar resultados en '.docx'.", align = "left"))
      ),
      br(),

      # --- Carga de datos ---
      h3("Cargar datos", align = "left"),
      br(),
      fileInput(
        "file3", "Importar datos del muestreo",
        accept = c(
          ".csv", ".txt", ".xlsx",
          "text/csv", "text/plain", "text/tab-separated-values",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      ),
      br(),
      uiOutput("variable_select_LES"),
      h4("IMPORTANTE: Debe seleccionar variables numéricas.", align = "left", style = "font-weight: bold"),
      uiOutput("negativesAlertMuestreoLES"),
      br(),

      # --- Tamaño y selección ---
      h2("Muestreo: tamaño y selección", align = "left"),
      br(),
      h4("Primero tamaño (tolerable, esperado, confianza) y luego unidades (PPS).", align = "left"),
      br(),

      h3("Cálculo de tamaño de muestra"),
      br(),
      h4("Guía rápida sobre parámetros:", align = "left"),
      h4("Margen de Tolerancia (Tolerable)", align = "left", style = "font-weight: bold"),
      h4("Error Esperado (Esperado)", align = "left", style = "font-weight: bold"),
      h4("Nivel de Confianza", align = "left", style = "font-weight: bold"),
      br(),

      h3("Tabla de sugerencia para determinar el tamaño de la muestra.", align = "left"),
      br(),
      h4("Recomendaciones por rangos (<50, 50–100, >100).", align = "left"),
      br(),

      fluidRow(
        shinydashboard::box(
          title = "Tabla de Datos",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          div(style = "height: 180px;", reactableOutput("SugerenciasTamaño_LES"))
        )
      ),

      # --- Parámetros y acción ---
      h4("Ajuste los parámetros y presione 'Análisis del muestreo'.", align = "left"),
      br(),
      sliderInput("freq1_LES", "Tolerable:", min = 0.01, max = 0.99, value = 0.05),
      sliderInput("freq2_LES", "Esperado:",  min = 0.01, max = 0.99, value = 0.01),
      selectInput("distri_2", "Seleccione el nivel:", list(`Tipo` = list("poisson", "binomial"))),
      h6("Importante: el 'Tolerable' debe ser superior al 'Esperado'."),
      sliderInput("freq3_LES", "Nivel de confianza:", min = 0.01, max = 0.99, value = 0.95),
      br(),
      h4("Valor LES.", align = "left"),
      numericInput("LES", "Valor del LES:", min = 0, value = 100000),

      conditionalPanel(
        condition = "(!output.hasNegatives_LES) && (input.freq2_LES < input.freq1_LES)",
        actionButton("update_LES", "Análisis del muestreo.", class = "btn-primary")
      ),
      br(), br(),

      # --- Tamaño de la muestra ---
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("SampleSize_LES")
        )
      ),
      br(),

      # --- Conteo según LES ---
      fluidRow(
        shinydashboard::box(
          title = "Conteo de Valores según LES",
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("ConteoLes")
        )
      ),

      # --- Semilla ---
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("seedvalue_LES")
        )
      ),
      br(), br(),

      # --- Muestra seleccionada ---
      fluidRow(
        shinydashboard::box(
          title = "Muestra Seleccionada",
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("MuestraLES")
        )
      ),
      br(),

      # --- Comparación ---
      h3("Comparación de datos cargados vs muestra seleccionada"),
      br(),
      fluidRow(
        shinydashboard::box(
          title = "Comparación de distribuciones entre datos cargados y la muestra",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8, highchartOutput("comp_dist_LES")
        )
      ),
      br(),

      # --- Descargas ---
      h3("Descargar la muestra seleccionada"),
      br(),
      actionButton("show1_LES", "Descargar archivo"),
      br(),
      h3("Descargar Reporte", align = "left"),
      conditionalPanel(                                           # NEW
        condition = "output.showDownloads",                       # NEW
        downloadButton("downloadReport3", "Descargar Reporte Muestreo LES")
      )
    ),

    #################################################################
    #                           PÁG. p5                             #
    #                     🧷 MUESTREO ATRIBUTOS                     #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p5",

      h1("Muestreo por atributos.", align = "center"),
      br(),

      h2("Se lleva a cabo el proceso de muestreo por atributos: tamaño y selección de las unidades.", align = "left"),
      br(),
      h4("Una vez cargada la información y seleccionadas las variables de atributo, usted podrá:"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Determinar niveles de error tolerable, esperado y nivel de confianza.", align = "left")),
        tags$li(h4("Determinar el tamaño de muestra.", align = "left")),
        tags$li(h4("Visualizar la muestra seleccionada.", align = "left")),
        tags$li(h4("Comparar porcentajes de categorías para originales vs muestra.", align = "left")),
        tags$li(h4("Descargar la muestra seleccionada.", align = "left")),
        tags$li(h4("Descargar los resultados en '.docx'.", align = "left"))
      ),
      br(),

      # --- Carga de datos ---
      h3("Cargar datos", align = "left"),
      br(),
      fileInput(
        "file4", "Importar datos del muestreo",
        accept = c(
          ".csv", ".txt", ".xlsx",
          "text/csv", "text/plain", "text/tab-separated-values",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      ),
      br(),
      uiOutput("variable_select_Atri"),
      h4("IMPORTANTE: Debe seleccionar variables de atributo.", align = "left", style = "font-weight: bold"),
      br(),

      # --- Tamaño y selección ---
      h2("Muestreo: tamaño y selección", align = "left"),
      br(),
      h4("Primero determine el tamaño (tolerable, esperado, confianza) y luego seleccione unidades.", align = "left"),
      br(),

      h3("Cálculo de tamaño de muestra"),
      br(),
      h4("Guía rápida sobre parámetros:", align = "left"),
      h4("Margen de Tolerancia (Tolerable)", align = "left", style = "font-weight: bold"),
      h4("Error Esperado (Esperado)", align = "left", style = "font-weight: bold"),
      h4("Nivel de Confianza", align = "left", style = "font-weight: bold"),
      br(),

      h3("Tabla de sugerencia para determinar el tamaño de la muestra.", align = "left"),
      br(),
      h4("Recomendaciones por rangos (<50, 50–100, >100).", align = "left"),
      br(),

      fluidRow(
        shinydashboard::box(
          title = "Tabla de Datos",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          div(style = "height: 180px;", reactableOutput("SugerenciasTamaño_Atri"))
        )
      ),

      h4("Ajuste parámetros y presione 'Análisis del muestreo'.", align = "left"),
      br(),
      sliderInput("freq1_Atri", "Tolerable:", min = 0.01, max = 0.99, value = 0.05),
      sliderInput("freq2_Atri", "Esperado:",  min = 0.01, max = 0.99, value = 0.01),
      selectInput("distri_3", "Seleccione el nivel:", list(`Tipo` = list("poisson", "binomial"))),
      h6("Importante: el 'Tolerable' debe siempre ser superior al 'Esperado'."),
      sliderInput("freq3_Atri", "Nivel de confianza:", min = 0.01, max = 0.99, value = 0.95),
      br(),

      conditionalPanel(
        condition = "(input.freq2_Atri < input.freq1_Atri)",
        actionButton("update_Atri", "Análisis del muestreo.", class = "btn-primary")
      ),
      br(), br(),

      # --- Tamaño de la muestra ---
      h3("Tamaño de la muestra.", align = "left"),
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("SampleSize_Atri")
        )
      ),
      br(),

      # --- Semilla ---
      h4("Semilla del proceso de selección.", align = "left"),
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("seedvalue_Atri")
        )
      ),
      br(), br(),

      # --- Muestra seleccionada ---
      h3("Muestra seleccionada.", align = "left"),
      fluidRow(
        shinydashboard::box(
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 8, reactableOutput("tablaMuestraAtri")
        )
      ),
      br(),

      # --- Comparación porcentual ---
      h3("Comparación porcentual entre datos originales y obtenidos por la muestra.", align = "left"),
      fluidRow(
        shinydashboard::box(
          title = "Gráfico Comparativo entre original y muestra",
          solidHeader = TRUE, status = "primary", collapsible = TRUE,
          width = 12, highchartOutput("graficoComparativo2")
        )
      ),
      br(),

      # --- Descargas ---
      h3("Descargar la muestra seleccionada"),
      br(),
      actionButton("show1_Atri", "Descargar archivo"),
      br(),
      h3("Descargar Reporte", align = "left"),
      conditionalPanel(                                           # NEW
        condition = "output.showDownloads",                       # NEW
        downloadButton("downloadReport4", "Descargar Reporte Muestreo Atributos")
      )
    ),

    #################################################################
    #                           PÁG. p6                             #
    #                          🧾 EVALUACIÓN                        #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p6",

      h1("Evaluación de la auditoría.", align = "center"),
      br(),

      h2("En esta sección:", align = "left"),
      br(),
      h4("Comparación observados vs auditados; indicadores y umbrales.", align = "left"),
      br(),
      tags$ul(
        style = .ul_style,
        tags$li(h4("Comparar observados vs auditados.", align = "left")),
        tags$li(h4("Valorar diferencias de forma descriptiva.", align = "left")),
        tags$li(h4("Analizar indicadores de riesgo.", align = "left")),
        tags$li(h4("Evaluar criterios empíricos (umbrales).", align = "left")),
        tags$li(h4("Descargar resultados en '.docx'.", align = "left"))
      ),
      br(),

      # --- Carga de datos ---
      h3("Cargar datos", align = "left"),
      fileInput(
        "file5", "Importar datos para la evaluación del muestreo.",
        accept = c(
          ".csv", ".txt", ".xlsx",
          "text/csv", "text/plain", "text/tab-separated-values",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      ),

      # --- Selección de variables ---
      h3("Seleccionar los parámetros para la evaluación de los valores observados y auditados."),
      br(),
      uiOutput("var1"),
      uiOutput("var2"),
      h4("IMPORTANTE: Debe seleccionar variables numéricas.", align = "left", style = "font-weight: bold"),
      br(),
      actionButton("analizar", "Evaluación", class = "btn-primary"),
      br(), br(),

      # --- Resultados descriptivos ---
      h2("Comparar la información de los datos observados vs. los datos auditados."),
      br(),
      h4("Datos, gráfico de dispersión y diferencias.", align = "left"),
      br(),

      fluidRow(
        shinydashboard::box(
          title = "Tabla de Datos",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          div(style = "height: 400px; overflow-y: auto;", reactableOutput("Tabla2"))
        ),
        div(style = "height: 30px;"),
        shinydashboard::box(
          title = "Gráfico de Dispersión",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          highchartOutput("ScatterPlot", height = "400px")
        ),
        shinydashboard::box(
          title = "Diferencias",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          div(style = "height: 400px; overflow-y: auto;", reactableOutput("Tabla3"))
        )
      ),

      h4("Descargar tabla de diferencias."),
      br(),
      actionButton("show2", "Descargar archivo"),
      br(), br(),

      # --- Indicadores de riesgo ---
      h2("Indicadores de riesgo en la comparación de datos."),
      br(),
      h4("Tabla de medidas y gráfico con intervalos de confianza.", align = "left"),
      br(),

      fluidRow(
        shinydashboard::box(
          title = "Indicadores de riesgo de evaluación",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          div(style = "height: 400px; overflow-y: auto;", reactableOutput("Riesgo"))
        ),
        shinydashboard::box(
          title = "Gráfico de Dispersión con intervalos de confianza",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          highchartOutput("ScatterPlot_limit", height = "400px")
        )
      ),

      br(),

      # --- Criterios empíricos ---
      h2("Criterio empírico del máximo umbral permitido o tolerado."),
      br(),
      h4("Seleccione límites permisibles y ejecute la evaluación.", align = "left"),
      br(),

      h3("Nota: en 'criterios de evaluación', seleccione valores y presione 'Evaluación'"),
      fluidRow(
        shinydashboard::box(
          title = "Criterios de Evaluación",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = 8,
          numericInput("monto_maximo", "Monto máximo tolerable:", min = 0, value = 5000),
          sliderInput("porcentaje_umbral", "Porcentaje máximo tolerado:", min = 0.01, max = 0.99, value = 0.15),
          sliderInput("conteo_umbral", "Conteo máximo de diferencias:", min = 0, max = 100, value = 15),
          sliderInput("casos_umbral", "Conteo máximo fuera de los límites de confianza:", min = 0, max = 100, value = 10),
          actionButton("auditEval", "Evaluación", class = "btn-primary")
        )
      ),

      fluidRow(
        conditionalPanel(
          condition = "input.auditEval > 0",
          shinydashboard::box(
            title = "Evaluación auditoría",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            width = 8, reactableOutput("Eval")
          )
        )
      ),

      br(),
      h3("Descargar Reporte", align = "left"),
      conditionalPanel(                                           # NEW
        condition = "output.showDownloads",                       # NEW
        downloadButton("downloadReport5", "Descargar Reporte Evaluación")
      )
    )

  ) # /tabItems
 # --- Tema oscuro + estilos de footer ---
  tags$head(
    # JS para agregar/quitar la clase .dark-mode en el <body>
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle-dark-mode', function(message) {
        if (message.active) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
    ")),
    # CSS básico para modo oscuro + footer
    tags$style(HTML("
      /* ------------------- Dark mode ------------------- */
      body.dark-mode {
        background-color: #0f172a;
        color: #e5e7eb;
      }
      body.dark-mode .skin-blue .main-header .navbar {
        background-color: #111827;
      }
      body.dark-mode .skin-blue .main-header .logo {
        background-color: #111827;
      }
      body.dark-mode .skin-blue .main-sidebar,
      body.dark-mode .skin-blue .left-side {
        background-color: #020617;
      }
      body.dark-mode .content-wrapper,
      body.dark-mode .right-side {
        background-color: #0f172a;
      }
      body.dark-mode .box {
        background-color: #020617;
        border-color: #1f2937;
      }
      body.dark-mode .box-header,
      body.dark-mode .box-title,
      body.dark-mode .box-body {
        color: #e5e7eb;
      }

      /* ------------------- Footer ------------------- */
      .app-footer {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        height: 26px;
        padding: 4px 18px;
        background-color: #f9fafb;
        border-top: 1px solid #e5e7eb;
        color: #6b7280;
        font-size: 11px;
        z-index: 1000;
      }
      body.dark-mode .app-footer {
        background-color: #020617;
        border-top-color: #1f2937;
        color: #9ca3af;
      }
      /* Para que el contenido no quede tapado por el footer */
      .content-wrapper {
        padding-bottom: 40px;
      }
    "))
  ),

  # ... aquí va TODO lo que ya tienes en el body (tabs, boxes, etc.) ...

  # --- Footer al final del body ---
  tags$footer(
    class = "app-footer",
    tags$span("Muestreo de Auditoría – Herramienta de apoyo al muestreo financiero"),
    tags$span(
      style = "float:right;",
      paste0("© ", format(Sys.Date(), '%Y'), " Contraloría General de la República")
    )
  )





)   # /dashboardBody