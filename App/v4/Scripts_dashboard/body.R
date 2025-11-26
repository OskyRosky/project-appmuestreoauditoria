###############################################################
# 📄 body.R — Contenido principal del dashboard (UI)
# -------------------------------------------------------------
# Cambios clave:
#  • Agregada franja de aviso cuando APP_HEAVY ≠ TRUE (usa output.showDownloads)  # NEW
#  • Los botones de descarga de reportes DOCX están dentro de conditionalPanel     # NEW
###############################################################

.ul_style <- "list-style-type: disc; padding-left: 20px;"

body <- shinydashboard::dashboardBody(

  # --- Tema oscuro + estilos de footer + estilos del sidebar (hover + active) ---
  tags$head(

    # JS para el modo oscuro
tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle-dark-mode', function(message) {
        if (message.active) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
    ")),

# 🟦 CSS - Dark Mode + Footer + Sidebar mejorado + Header/Sidebar custom
tags$style(HTML("

      /* --------------------------------------------------------- */
      /*                      🌞 MODO CLARO: COLORES               */
      /* --------------------------------------------------------- */

      /* Header (barra superior) */
      .skin-blue .main-header .navbar {
        background-color: #0ea5e9 !important;  /* 🔵 color del header */
      }

      .skin-blue .main-header .logo {
        background-color: #0ea5e9 !important;  /* 🔵 mismo color */
      }

      /* Sidebar (barra izquierda) */
      .skin-blue .main-sidebar,
      .skin-blue .left-side {
        background-color: #111827 !important;  /* ⚫ gris oscuro */
      }

      /* Texto de los items del sidebar */
      .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a {
        color: #e5e7eb !important;
      }


      /* --------------------------------------------------------- */
      /*                      🌙 MODO OSCURO                       */
      /* --------------------------------------------------------- */

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

      body.dark-mode .main-sidebar,
      body.dark-mode .left-side {
        background-color: #020617 !important;
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


      /* --------------------------------------------------------- */
      /*                          🦶 FOOTER                        */
      /* --------------------------------------------------------- */

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


      /* --------------------------------------------------------- */
      /*               ⭐ SIDEBAR: Hover + Active full box         */
      /* --------------------------------------------------------- */

      .skin-blue .main-sidebar .sidebar .sidebar-menu > li.active > a {
        background-color: #0c4a6e !important;
        color: #ffffff !important;
        border-radius: 4px;
        margin: 4px 8px;
      }

      .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:hover {
        background-color: #0c4a6e !important;
        color: #ffffff !important;
        border-radius: 4px;
        margin: 4px 8px;
      }

")),

# 🔥 BLOQUE NUEVO PARA SIDEBAR “MINI” (solo íconos cuando se colapsa)
tags$style(HTML("

      /* Sidebar colapsado en modo MINI (tipo ShinyLEGO) */

      .sidebar-collapse .main-sidebar {
        transform: none !important;
        width: 60px !important;
      }

      .sidebar-collapse .left-side {
        width: 60px !important;
      }

      /* Mantener margen del contenido */
      .sidebar-collapse .content-wrapper,
      .sidebar-collapse .main-footer {
        margin-left: 60px !important;
      }

      /* Ocultar texto, dejar solo iconos */
      .sidebar-collapse .sidebar-menu > li > a > span {
        display: none !important;
      }

      .sidebar-collapse .sidebar-menu > li > a {
        text-align: center !important;
      }

      .sidebar-collapse .sidebar-menu > li > a > .fa {
        margin-right: 0 !important;
      }

"))
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
  br(),
  br(),
  tags$div(
    style = "text-align:center; padding-top:10px;",
    tags$img(
      src = "ima/muestreo_auditoria.png",
      style = "max-width:100%; 
               max-height:460px; 
               height:auto; 
              border-radius:10px;"
    )
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

  # --- Reporte clásico (DOCX con officer/flextable) ---
  h3("Descargar Reporte", align = "left"),
  conditionalPanel(
    condition = "output.showDownloads",
    downloadButton("downloadReport1", "Descargar Reporte Análisis Descriptivo")
  ),

  br(), hr(),

  # =====================================================
  # 🧠 NUEVO BLOQUE: Informe automatizado con LLM
  # =====================================================

      # --- Informe automatizado (LLM) ---
      h3("Informe automatizado (LLM)", align = "left"),
      tags$p(
        "Opcionalmente, la aplicación puede redactar un informe breve y conclusivo ",
        "sobre los resultados descriptivos utilizando un modelo de lenguaje (LLM)."
      ),

      # Contexto que el usuario aporta al modelo
      textAreaInput(
        inputId   = "p2_llm_context",
        label     = "Explique la temática, la entidad o empresa auditada y el objetivo del análisis descriptivo:",
        placeholder = "Ejemplo: Análisis descriptivo de la cartera de cuentas por cobrar del Ministerio X, para evaluar concentración de saldos y apoyar la planificación de pruebas sustantivas.",
        rows      = 4
      ),

      actionButton(
        inputId = "p2_llm_generate",
        label   = "Generar informe con LLM",
        class   = "btn-success"
      ),
      br(), br(),

      h4("Borrador de informe generado:"),
      verbatimTextOutput("p2_llm_preview"),

      # Botón de descarga (se mostrará solo cuando haya texto)
      shinyjs::hidden(
        downloadButton(
          outputId = "p2_llm_docx",
          label    = "Descargar informe LLM (.docx)"
        )
      )
),

    #################################################################
    #                           PÁG. p3                             #
    #                         🧮 MUESTREO MUM                       #
    #################################################################
    shinydashboard::tabItem(
      tabName = "p3",

      h1("Muestreo MUM.", align = "center"),
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

      h1("Muestreo para atributos.", align = "center"),
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

      h1("Evaluación muestreo en auditoría.", align = "center"),
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
    ),

shinydashboard::tabItem(
      tabName = "faq",

      h1("Preguntas frecuentes (FAQ)", align = "center"),
      br(),

      fluidRow(
        shinydashboard::box(
          title = "¿Qué formatos de archivo acepta la aplicación?",
          status = "primary", solidHeader = TRUE, width = 12,
          p("Puede cargar archivos en formato ",
            strong(".xlsx, .csv o .txt"),
            ". Cada archivo debe contener una sola tabla y estar limpio para el análisis.")
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "¿Cuál es el tamaño máximo permitido por archivo?",
          status = "primary", solidHeader = TRUE, width = 12,
          p("El peso máximo recomendado por archivo es de ",
            strong("100 MB"),
            ", para mantener un desempeño fluido en la aplicación.")
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "¿En qué módulos puedo generar reportes en .docx?",
          status = "primary", solidHeader = TRUE, width = 12,
          p("Se pueden generar reportes en formato ",
            strong(".docx"),
            " en los módulos de:"),
          tags$ul(
            tags$li("Análisis descriptivo"),
            tags$li("Muestreo MUM"),
            tags$li("Muestreo LES"),
            tags$li("Muestreo por atributos"),
            tags$li("Evaluación de la muestra")
          )
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "¿Qué hago si la aplicación no me deja descargar el reporte?",
          status = "primary", solidHeader = TRUE, width = 12,
          p("Verifique que la aplicación esté ejecutándose en modo pesado ",
            code("APP_HEAVY = TRUE"),
            " y que los paquetes ",
            code("officer"), " y ", code("flextable"),
            " estén instalados en el entorno de R.")
        )
      )
    )


  ), # /tabItems

  # --- Footer al final del body ---
  tags$footer(
    class = "app-footer",
    tags$span("Muestreo de Auditoría – Herramienta de apoyo al muestreo en Auditoría."),
    tags$span(
      style = "float:right;",
      paste0("© ", format(Sys.Date(), '%Y'), " App Muestreo Ejemplo")
    )
  )

)   # /dashboardBody