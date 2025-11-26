############################
#   Contenido del server   #
############################
# Estructura general:
#  0) Helpers & estado global (reactiveValues)
#  1) Análisis descriptivo (p1 / file1)
#  2) Muestreo MUM (p3 / file2)
#  3) Muestreo LES (p4 / file3)
#  4) Muestreo por Atributos (p5 / file4)
#  5) Evaluación (p6 / file5)
#
# IMPORTANTE:
# - No se cambian IDs referenciados por el UI.
# - Se eliminan duplicados de objetos (sample_size, reactive_seed, renderUI).
# - Se encapsulan tareas repetidas en funciones auxiliares.

server <- function(input, output, session) {

  # =====================================================================
  # 0) HELPERS & ESTADO GLOBAL
  # =====================================================================

  # ---- 0.1 Utilidades de lectura (CSV/TXT/XLSX) -----------------------
  .read_any <- function(path) {
    ext <- tools::file_ext(path)
    switch(ext,
      csv  = utils::read.csv(path, stringsAsFactors = FALSE),
      txt  = utils::read.delim(path, stringsAsFactors = FALSE),
      xlsx = readxl::read_excel(path),
      stop("Tipo de archivo no soportado")
    )
  }

# --- Helper XLSX seguro ---
.sanitize_for_xlsx <- function(df) {
  if (inherits(df, "tbl_df")) df <- as.data.frame(df)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.list(x)) {
      df[[nm]] <- vapply(x, function(v) {
        if (length(v) == 0) "" else paste(as.character(v), collapse = ", ")
      }, FUN.VALUE = character(1))
    } else if (is.factor(x)) {
      df[[nm]] <- as.character(x)
    } else if (inherits(x, "POSIXt") || inherits(x, "Date")) {
      df[[nm]] <- as.character(x)
    }
  }
  df
}

# --- Helper para gráficos de densidad con highcharter ---
.hc_density <- function(x, name = "Variable", color = "#1f77b4") {
  if (!requireNamespace("highcharter", quietly = TRUE)) {
    stop("Falta el paquete 'highcharter'.")
  }

  # Calcula densidad
  dens <- stats::density(x, na.rm = TRUE)
  df <- data.frame(x = dens$x, y = dens$y)

  # Construye gráfico interactivo
  highcharter::highchart() |>
    highcharter::hc_chart(type = "areaspline") |>
    highcharter::hc_add_series(
      data = list_parse2(df),
      name = name,
      color = color,
      fillOpacity = 0.5
    ) |>
    highcharter::hc_title(text = "Distribución de Densidad") |>
    highcharter::hc_xAxis(title = list(text = "Valor")) |>
    highcharter::hc_yAxis(title = list(text = "Densidad")) |>
    highcharter::hc_tooltip(pointFormat = "Valor: {point.x:.2f}<br>Densidad: {point.y:.4f}") |>
    highcharter::hc_exporting(enabled = TRUE)
}

  # ---- 0.2 Validaciones cortas ----------------------------------------
  .need_numeric <- function(df, var, msg = "Seleccione una variable numérica válida.") {
    validate(need(!is.null(var) && var %in% names(df), "Seleccione una variable."))
    validate(need(is.numeric(df[[var]]), msg))
  }

# ---- 0.3 Helpers Highcharter (FIX) -----------------------------------
.hc_density_compare <- function(x1, x2, name1, name2,
                                col1 = "skyblue", col2 = "green",
                                title = "Comparación de Densidades") {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  if (length(x1) < 2 || length(x2) < 2) {
    # Chart mínimo y mensaje si no hay suficientes datos
    return(
      highcharter::highchart() |>
        highcharter::hc_title(text = "No hay datos suficientes para comparar densidades") |>
        highcharter::hc_chart(zoomType = "xy")
    )
  }

  d1 <- stats::density(x1, na.rm = TRUE)
  d2 <- stats::density(x2, na.rm = TRUE)

  # Convertimos a pares (x, y) para highcharter
  s1 <- highcharter::list_parse2(data.frame(x = d1$x, y = d1$y))
  s2 <- highcharter::list_parse2(data.frame(x = d2$x, y = d2$y))

  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = s1, type = "area", name = name1, color = col1
    ) |>
    highcharter::hc_add_series(
      data = s2, type = "area", name = name2, color = col2
    ) |>
    highcharter::hc_title(text = title) |>
    highcharter::hc_tooltip(crosshairs = TRUE, valueDecimals = 3,
                            shared = TRUE, borderWidth = 5) |>
    highcharter::hc_chart(zoomType = "xy") |>
    highcharter::hc_exporting(enabled = TRUE)
}

  # ---- 0.4 Helpers Reportes (ggsave + officer) ------------------------
  .ggsave_tmp <- function(plot, w = 7, h = 5, dpi = 300) {
    f <- tempfile(fileext = ".png")
    ggplot2::ggsave(f, plot = plot, width = w, height = h, dpi = dpi)
    f
  }

  # ---- 0.5 Estado global para evitar colisiones -----------------------
  rv <- reactiveValues(
    # Descriptivo
    data1 = NULL,
    # MUM
    data2 = NULL,
    sample_size_mum = NULL,
    seed_mum = NULL,
    muestra_mum = NULL,
    neg_mum = FALSE,
    # LES
    data3 = NULL,
    sample_size_les = NULL,
    seed_les = NULL,
    muestra_les = NULL,
    neg_les = FALSE,
    # ATRI
    data4 = NULL,
    sample_size_atri = NULL,
    seed_atri = NULL,
    muestra_atri = NULL,
    # EVAL
    data5 = NULL,
    eval_table = NULL,
    eval_diffs = NULL,
    eval_decision = NULL
  )

  # ---- 0.6 Flag de modo "pesado" (control de descargas DOCX) ----------
  is_heavy <- identical(tolower(Sys.getenv("APP_HEAVY", "true")), "true")
  output$showDownloads <- reactive({ is_heavy })
  outputOptions(output, "showDownloads", suspendWhenHidden = FALSE)

# ============================================================
#  🔁 Botón del header dinámico según el tab del sidebar
# ============================================================
output$header_help_button <- renderUI({
  current_tab <- input$sidebar

  # Texto del botón según la pestaña activa
  label_text <- switch(
    current_tab,
    p1  = "¡ Bienvenido !",
    p2  = "Descripción",
    p3  = "Sobre MUM",
    p4  = "Sobre LES",
    p5  = "Sobre Atributos",
    p6  = "Sobre Evaluación",
    faq = "FAQs",
    "¡ Bienvenido !"   # valor por defecto
  )

  actionButton(
    inputId = "welcome_guide",
    label = tagList(
      icon("info-circle"),
      tags$span(
        label_text,
        style = "margin-left:6px; font-size:16px; font-weight:600;"
      )
    ),
    class = "btn btn-success",
    style = "padding:10px 28px;
             font-size:16px;
             border-radius:8px;"
  )
})

# -------------------------------------------------------------------
# Helper: validar que haya datos y que la variable sea numérica
# -------------------------------------------------------------------
.need_numeric <- function(df, var) {
  shiny::validate(
    shiny::need(!is.null(df) && nrow(df) > 0,
                "Debe cargar un archivo de datos antes de continuar."),
    shiny::need(!is.null(var) && nzchar(var) && var %in% names(df),
                "Debe seleccionar una variable disponible en el archivo."),
    shiny::need(is.numeric(df[[var]]),
                "La variable seleccionada debe ser numérica.")
  )
}

# =========================================================
# (7) Helper LLM: llamada a Ollama desde R
# ---------------------------------------------------------
# Usa variables de entorno si quieres cambiar modelo/host:
#   OLLAMA_HOST  (ej. "http://localhost:11434")
#   OLLAMA_MODEL (ej. "llama3.3:latest")
# =========================================================
ollama_generate <- function(prompt,
                            model = Sys.getenv("OLLAMA_MODEL", "llama3.3:latest"),
                            host  = Sys.getenv("OLLAMA_HOST",  "http://localhost:11434")) {

  stopifnot(is.character(prompt), length(prompt) == 1)

  url  <- paste0(host, "/api/generate")

  body <- list(
    model  = model,
    prompt = prompt,
    stream = FALSE
  )

  resp <- httr2::request(url) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  out <- httr2::resp_body_json(resp)

  # En la API de Ollama, el texto viene en el campo "response"
  out$response
}

# ============================================================
# 🟢 Modal de ayuda dependiendo del módulo actual
#     - p1  → guía COMPLETA (tu versión original, intacta)
#     - p2+ → guías resumidas por módulo
# ============================================================

observeEvent(input$welcome_guide, {

  current_tab <- input$sidebar
  if (is.null(current_tab)) current_tab <- "p1"


  # ------------------------------------------------------------
  # 1) SI ESTÁS EN p1 → MOSTRAR LA GUÍA COMPLETA ORIGINAL
  # ------------------------------------------------------------

 if (current_tab == "p1") {

  showModal(
    modalDialog(
      title = "Guía general de la aplicación",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Cerrar"),

      tags$div(
        style = "max-height:60vh; overflow-y:auto; font-size:13px;",

        # ----- Introducción -----
        tags$h4("Introducción"),
        tags$p(
          "Bienvenidos(as) a la aplicación especializada en el análisis de muestras para unidades monetarias."
        ),
        tags$p(
          "Esta herramienta interactiva ha sido diseñada para facilitar el proceso de ",
          tags$strong("descripción, muestreo y evaluación"),
          " de poblaciones monetarias en el contexto de la auditoría."
        ),

        # ----- Cómo iniciar -----
        tags$h4("¡Inicie utilizando la aplicación!"),
        tags$p("Para comenzar, en cada sección de la barra lateral izquierda deberá:"),
        tags$ul(
          tags$li(
            tags$strong("Navegar: "),
            "utilice las pestañas del menú lateral para moverse entre los distintos módulos."
          ),
          tags$li(
            tags$strong("Cargar datos: "),
            "use el botón gris de ",
            tags$em("Cargar datos"),
            " en cada módulo para subir su archivo."
          ),
          tags$li(
            tags$strong("Analizar: "),
            "siga las instrucciones específicas en cada sección para ejecutar el análisis requerido."
          )
        ),

        # ----- Estructura de la aplicación -----
        tags$h4("Estructura de la aplicación"),
        tags$p(
          "La aplicación se organiza en módulos, cada uno enfocado en un aspecto clave del muestreo en unidades monetarias:"
        ),
        tags$ul(
          tags$li(tags$strong("Análisis descriptivo:"), " comprensión inicial de la población."),
          tags$li(tags$strong("Proceso de muestreo (MUM y LES):"), " diseño y selección de la muestra monetaria."),
          tags$li(tags$strong("Muestreo por atributos:"), " enfoque para variables cualitativas o de cumplimiento."),
          tags$li(tags$strong("Evaluación de la muestra:"), " contraste empírico entre lo observado y lo auditado.")
        ),
        tags$p("A continuación, se resume el objetivo de cada módulo."),

        # ----- Análisis descriptivo -----
        tags$h4("Análisis descriptivo"),
        tags$ul(
          tags$li("Analizar las principales estadísticas descriptivas de la variable de interés (totales, promedios, dispersión)."),
          tags$li("Examinar la distribución de la variable para identificar sesgos, concentración y valores atípicos."),
          tags$li("Explorar ajustes de funciones de distribución (por ejemplo, Normal, Poisson o Binomial) como insumo para el diseño muestral.")
        ),

        # ----- Proceso de muestreo (MUM y LES) -----
        tags$h4("Proceso de muestreo (MUM y LES)"),
        tags$ul(
          tags$li("Determinar el tamaño de muestra según parámetros de riesgo, materialidad y criterios técnicos."),
          tags$li("Visualizar la selección de unidades monetarias que conforman la muestra."),
          tags$li("Comparar la distribución de la población original con la de la muestra obtenida."),
          tags$li("Descargar la tabla de la muestra para su trabajo de auditoría de detalle.")
        ),

        # ----- Muestreo por atributos -----
        tags$h4("Muestreo por atributos"),
        tags$ul(
          tags$li("Diseñar muestras cuando la variable de interés es categórica (cumple / no cumple, sí / no, etc.)."),
          tags$li("Calcular el tamaño de muestra con base en proporciones esperadas de error y niveles de confianza."),
          tags$li("Visualizar los elementos seleccionados y comparar porcentajes entre población y muestra."),
          tags$li("Descargar los datos de la muestra para su documentación en papeles de trabajo.")
        ),

        # ----- Evaluación de la muestra -----
        tags$h4("Evaluación de la muestra"),
        tags$ul(
          tags$li("Comparar valores observados (población o registros contables) versus valores auditados."),
          tags$li("Describir y valorar las diferencias detectadas mediante indicadores de error y riesgo."),
          tags$li("Definir y aplicar criterios o umbrales empíricos para concluir sobre la aceptabilidad de la población.")
        ),

        # ----- Reportes -----
        tags$h4("Reportes de análisis"),
        tags$p(
          "Cada módulo cuenta con un botón de ",
          tags$strong("'Descargar reporte'"),
          ", que genera un documento en formato ",
          tags$code(".docx"),
          " con los resultados más relevantes para anexar a los papeles de trabajo."
        ),

        # ----- Sobre carga de datos -----
        tags$h4("Sobre la carga de datos"),
        tags$ul(
          tags$li(
            tags$strong("Formatos permitidos: "),
            tags$code(".xlsx"), ", ",
            tags$code(".csv"), " y ",
            tags$code(".txt"),
            "."
          ),
          tags$li("Cada archivo debe contener una sola tabla y estar previamente depurado."),
          tags$li(
            "El tamaño máximo ",
            tags$strong("recomendado"),
            " por archivo es de ",
            tags$strong("100 MB"),
            ", para mantener un desempeño fluido en la aplicación."
          )
        )
      )
    )
  )

  return()   # 👈 PARA QUE NO AVANCE A LOS OTROS MÓDULOS
}



  # ------------------------------------------------------------
  # 2) SI NO ES p1 → GUÍAS CORTAS SEGÚN LA SECCIÓN
  # ------------------------------------------------------------
  modal_title <- switch(
    current_tab,
    p2  = "Guía del módulo Descriptivo",
    p3  = "Guía del módulo Muestra MUM",
    p4  = "Guía del módulo Muestra LES",
    p5  = "Guía del módulo Muestra Atributos",
    p6  = "Guía del módulo Evaluación",
    faq = "Preguntas frecuentes",
    "Guía rápida"
  )

  modal_body <- switch(
    current_tab,

    # ---------- p2 ----------
    p2 = tagList(
  tags$h4("Módulo Descriptivo"),

  tags$p(
    "Este módulo permite analizar de forma inicial la población completa de la variable monetaria seleccionada. ",
    "Su objetivo es brindar una comprensión clara del comportamiento de los datos antes de avanzar a las etapas de muestreo."
  ),

  tags$ul(
    tags$li("Cargar la población completa correspondiente al universo de auditoría."),
    tags$li("Explorar estadísticas descriptivas clave: totales, promedios, mediana, desviación estándar y concentración."),
    tags$li("Visualizar distribuciones de frecuencias e identificar patrones relevantes, como asimetrías o valores atípicos."),
    tags$li("Comparar ajustes de distribución (Normal, Poisson y Binomial) para apoyar la selección del método de muestreo."),
    tags$li(
      tagList(
        "Generar un reporte en formato ",
        tags$code(".docx"),
        " con los resultados descriptivos más relevantes."
      )
    )
  )
),

    # ---------- p3 ----------
    p3 = tagList(
  tags$h4("Módulo Muestra MUM"),

  tags$p(
    "En este módulo se realiza el muestreo por unidades monetarias (MUM), ",
    "diseñado para trabajar con datos que representan montos o importes. ",
    "Su objetivo es seleccionar una muestra monetaria representativa de la población bajo auditoría."
  ),

  tags$ul(
    tags$li("Cargar la base de datos con los montos que conforman la población a auditar."),
    tags$li("Calcular el tamaño de muestra en función de los parámetros definidos (riesgo, materialidad, etc.)."),
    tags$li("Visualizar las unidades monetarias seleccionadas como parte de la muestra."),
    tags$li("Comparar la estructura de la población original frente a la muestra seleccionada (distribución y concentración de montos)."),
    tags$li(
      tagList(
        "Descargar la muestra seleccionada en formatos ",
        tags$code(".csv"), ", ",
        tags$code(".txt"), " o ",
        tags$code(".xlsx"),
        " para su uso en el trabajo de auditoría."
      )
    ),
    tags$li(
      tagList(
        "Generar y descargar un informe en formato ",
        tags$code(".docx"),
        " con los resultados y conclusiones más relevantes de esta sección, listo para incorporarse a los papeles de trabajo."
      )
    )
  )
),

    # ---------- p4 ----------
    p4 = tagList(
  tags$h4("Módulo Muestra LES"),

  tags$p(
    "En este módulo se aplica el método de muestreo basado en el ",
    tags$strong("Límite de Error Superior (LES)"),
    ", una técnica utilizada cuando los importes monetarios requieren un criterio de tolerancia ",
    "explícito para definir la magnitud aceptable del error en auditoría."
  ),

  tags$p(
    "El proceso está diseñado para determinar un tamaño de muestra coherente con el criterio definido ",
    "por el auditor y generar una selección que represente adecuadamente la población monetaria."
  ),

  tags$ul(
    tags$li("Cargar la población completa de montos que serán evaluados mediante el criterio LES."),
    tags$li(
      tagList(
        "Definir el ",
        tags$strong("criterio LES"),
        ", el cual corresponde a una cifra que establece el límite de error permitido en la evaluación."
      )
    ),
    tags$li("Calcular el tamaño de muestra requerido según el criterio seleccionado."),
    tags$li("Visualizar las unidades seleccionadas como parte de la muestra LES."),
    tags$li("Comparar la distribución de la población original con la distribución de la muestra generada."),
    tags$li(
      tagList(
        "Descargar la muestra seleccionada en formatos ",
        tags$code(".csv"), ", ",
        tags$code(".txt"), " o ",
        tags$code(".xlsx"),
        " para su utilización en auditoría."
      )
    ),
    tags$li(
      tagList(
        "Generar un informe en formato ",
        tags$code(".docx"),
        " con los principales resultados y comparaciones realizadas, ",
        "listo para incluirse en los papeles de trabajo."
      )
    )
  )
),

    # ---------- p5 ----------
    p5 = tagList(
  tags$h4("Módulo Muestra por Atributos"),

  tags$p(
    "Este módulo está diseñado para aplicar ",
    tags$strong("muestreo por atributos"),
    ", enfocado en variables cualitativas o categóricas ",
    "(por ejemplo: cumple / no cumple, sí / no, conforme / no conforme)."
  ),

  tags$p(
    "El objetivo es estimar proporciones poblacionales a partir de una muestra, ",
    "evaluando si la tasa de error observada se mantiene dentro de los niveles ",
    "tolerables definidos por el auditor."
  ),

  tags$ul(
    tags$li(
      "Cargar la población completa de la variable categórica o de cumplimiento ",
      "que será objeto de auditoría."
    ),
    tags$li(
      "Calcular el ",
      tags$strong("tamaño de muestra"),
      " en función de parámetros como proporción esperada de error, ",
      "nivel de confianza y error tolerable."
    ),
    tags$li(
      "Visualizar las unidades seleccionadas como parte de la muestra de atributos."
    ),
    tags$li(
      "Comparar las proporciones observadas en la muestra con las proporciones poblacionales, ",
      "identificando diferencias relevantes entre lo esperado y lo estimado."
    ),
    tags$li(
      tagList(
        "Descargar la muestra seleccionada en formatos ",
        tags$code(".csv"), ", ",
        tags$code(".txt"), " o ",
        tags$code(".xlsx"),
        " para su uso en el trabajo de auditoría."
      )
    ),
    tags$li(
      tagList(
        "Generar un informe en formato ",
        tags$code(".docx"),
        " con los resultados principales del muestreo por atributos, ",
        "incluyendo tamaños de muestra, proporciones estimadas y conclusiones clave ",
        "para los papeles de trabajo."
      )
    )
  )
),

    # ---------- p6 ----------
    p6 = tagList(
  tags$h4("Módulo Evaluación de la Muestra"),

  tags$p(
    "Este módulo permite realizar la ",
    tags$strong("evaluación empírica de la muestra monetaria"),
    " mediante la comparación entre los valores ",
    tags$em("observados"),
    " y los valores ",
    tags$em("auditados"),
    " durante el trabajo de detalle."
  ),

  tags$p(
    "El propósito es determinar si las diferencias encontradas en la muestra ",
    "representan riesgos materiales para la población completa, ",
    "y si los resultados cumplen con los umbrales definidos por el auditor."
  ),

  tags$ul(
    tags$li(
      "Cargar los resultados auditados correspondientes a la muestra previamente seleccionada."
    ),
    tags$li(
      "Comparar los valores observados versus auditados, identificando diferencias absolutas y relativas."
    ),
    tags$li(
      "Analizar de forma descriptiva las diferencias: montos, comportamiento de las desviaciones y su concentración."
    ),
    tags$li(
      "Calcular indicadores de riesgo y error proyectado para determinar la aceptabilidad de la población auditada."
    ),
    tags$li(
      "Evaluar criterios o umbrales empíricos definidos por el auditor, ",
      "determinando si la muestra respalda una conclusión razonable sobre la población."
    ),
    tags$li(
      tagList(
        "Descargar los resultados de evaluación en formato ",
        tags$code(".docx"),
        ", incluyendo tablas comparativas, indicadores de riesgo y conclusiones clave ",
        "para los papeles de trabajo."
      )
    )
  )
),

    # ---------- FAQ ----------
    faq = tagList(
  tags$h4("Preguntas frecuentes (FAQ)"),

  tags$ul(

    tags$li(
      tags$strong("¿Qué formatos de archivo acepta la aplicación?"),
      tags$p(
        "Puede cargar archivos en formato ",
        tags$code(".xlsx"), ", ",
        tags$code(".csv"), " o ",
        tags$code(".txt"), 
        ". Cada archivo debe contener una sola tabla y estar limpio para su análisis."
      )
    ),

    tags$li(
      tags$strong("¿Cuál es el tamaño máximo recomendado para los archivos?"),
      tags$p(
        "Se recomienda un peso máximo de ",
        tags$strong("100 MB"),
        " por archivo, para garantizar un desempeño fluido en la aplicación."
      )
    ),

    tags$li(
      tags$strong("¿En qué módulos puedo generar reportes en .docx?"),
      tags$p("Los reportes en formato .docx pueden generarse en:"),
      tags$ul(
        tags$li("Análisis descriptivo"),
        tags$li("Muestreo MUM"),
        tags$li("Muestreo LES"),
        tags$li("Muestreo por atributos"),
        tags$li("Evaluación de la muestra")
      )
    ),

    tags$li(
      tags$strong("¿Por qué la aplicación no me permite descargar el reporte?"),
      tags$p(
        "Verifique que la aplicación se esté ejecutando en modo pesado ",
        tags$code("APP_HEAVY = TRUE"),
        " y que los paquetes ",
        tags$code("officer"),
        " y ",
        tags$code("flextable"),
        " estén instalados en su entorno de R."
      )
    )
  )
),

    # ---------- Default ----------
    tagList(tags$p("Guía rápida del módulo actual."))
  )

  showModal(
    modalDialog(
      title     = modal_title,
      size      = "m",
      easyClose = TRUE,
      footer    = modalButton("Cerrar"),
      modal_body
    )
  )

})


  # ============================================================
  #  🌗 Modo oscuro: envía el estado al frontend (JS)
  # ============================================================
  observe({
    session$sendCustomMessage(
      "toggle-dark-mode",
      list(active = isTRUE(input$dark_mode))
    )
  })

  # =====================================================================
  # 1) ANÁLISIS DESCRIPTIVO (p2)  - fileInput: file1
  # =====================================================================

  # ---- 1.1 Lectura de datos -------------------------------------------
  data1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    rv$data1 <- .read_any(inFile$datapath)
    rv$data1
  })

  # ---- 1.2 Selector de variable + alerta negativos ---------------------
  output$variable_select_1 <- renderUI({
    if (is.null(data1())) return(NULL)
    selectInput("variable1", "Elija una variable:", names(data1()))
  })

  has_negatives_1 <- reactiveVal(FALSE)

  observe({
    req(data1(), input$variable1)
    v <- data1()[[input$variable1]]
    if (is.numeric(v) && any(v < 0, na.rm = TRUE)) has_negatives_1(TRUE) else has_negatives_1(FALSE)
  })

  output$negativesAlert_1 <- renderUI({
    if (has_negatives_1()) {
      tags$div(class = "alert alert-danger",
               strong("¡Se detectaron valores negativos!"),
               " Verifique si es correcto. Caso contrario, proceda a tomar medidas.")
    }
  })
  outputOptions(output, 'negativesAlert_1', suspendWhenHidden = FALSE)

  # ---- 1.3 Datos demo Binomial / Poisson -------------------------------
  set.seed(123)
  datos_binom <- rbinom(n = 10000, size = 100, prob = 0.5)
  datos_pois_extremos <- c(rpois(10000, lambda = 40), sample(80:100, size = 10, replace = TRUE))

  # ---- 1.4 Renderizaciones tras “Iniciar Análisis Descriptivos” --------
  observeEvent(input$start_analysis, {

    # 1.4.1 Tabla de estadísticas
    output$stats <- reactable::renderReactable({
      req(data1(), input$variable1)
      .need_numeric(data1(), input$variable1)
      Datos <- data.frame(Monto = data1()[[input$variable1]])
      Stats <- dplyr::summarise(
        Datos,
        ConteoCasos = sum(!is.na(Monto)),
        ValoresNegativos = sum(Monto < 0, na.rm = TRUE),
        ValoresFaltantes = sum(is.na(Monto)),
        Minimo  = min(Monto, na.rm = TRUE),
        Maximo  = max(Monto, na.rm = TRUE),
        Promedio = mean(Monto, na.rm = TRUE),
        Mediana  = median(Monto, na.rm = TRUE),
        Moda     = as.numeric(names(sort(table(Monto), decreasing = TRUE)[1])),
        DesviacionEstandar = stats::sd(Monto, na.rm = TRUE),
        Percentil10 = stats::quantile(Monto, 0.1, na.rm = TRUE),
        Percentil25 = stats::quantile(Monto, 0.25, na.rm = TRUE),
        Percentil50 = stats::quantile(Monto, 0.50, na.rm = TRUE),
        Percentil75 = stats::quantile(Monto, 0.75, na.rm = TRUE),
        Percentil90 = stats::quantile(Monto, 0.90, na.rm = TRUE)
      ) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Medida", values_to = "Valor") |>
        dplyr::mutate(Valor = round(Valor, 1))

      reactable::reactable(Stats, defaultPageSize = 15)
    })

    # 1.4.2 Densidad (Highcharter)
    output$histogram1 <- highcharter::renderHighchart({
      req(data1(), input$variable1)
      .need_numeric(data1(), input$variable1)
      .hc_density(data1()[[input$variable1]], name = "Monto", color = "skyblue")
    })

    # 1.4.3 Figuras demo
    output$binomialPlot <- renderPlot({
      ggplot2::ggplot(data.frame(Valor = datos_binom), ggplot2::aes(x = Valor)) +
        ggplot2::geom_histogram(binwidth = 1, fill = 'skyblue', color = 'black') +
        ggplot2::labs(title = "Distribución Binomial", x = "", y = "Frecuencia")
    })

    output$poissonPlot <- renderPlot({
      ggplot2::ggplot(data.frame(Valor = datos_pois_extremos), ggplot2::aes(x = Valor)) +
        ggplot2::geom_histogram(bins = 120, fill = 'skyblue', color = 'black') +
        ggplot2::labs(title = "Distribución de Poisson con valores extremos", x = "", y = "Frecuencia")
    })
  })

  # ---- 1.5 Reporte descriptivo (.docx) ---------------------------------
output$downloadReport1 <- downloadHandler(
  filename = function() paste0("Reporte_Analisis_Descriptivo_", Sys.Date(), ".docx"),
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  content = function(file) {
    tryCatch({
      req(data1(), input$variable1); .need_numeric(data1(), input$variable1)

      # Asegura que officer y flextable estén disponibles
      if (!requireNamespace("officer", quietly = TRUE) ||
          !requireNamespace("flextable", quietly = TRUE)) {
        stop("Faltan paquetes 'officer' y/o 'flextable'.")
      }

      # 1) Portada y metadatos
      doc <- officer::read_docx() |>
        officer::body_add_par("Análisis Descriptivo", style = "heading 1") |>
        officer::body_add_par(paste("Archivo de datos:", input$file1$name), style = "heading 2") |>
        officer::body_add_par(paste("Variable seleccionada:", input$variable1), style = "heading 2")

      # 2) Tabla de estadísticas
      var_data <- data1()[[input$variable1]]
      Stats <- tibble::tibble(
        Medida = c("Conteo de Casos","Valores Negativos","Valores Faltantes","Mínimo","Máximo","Promedio",
                   "Mediana","Moda","Desviación Estándar","Percentil 10","Percentil 25","Percentil 50","Percentil 75","Percentil 90"),
        Valor = c(
          sum(!is.na(var_data)),
          sum(var_data < 0, na.rm = TRUE),
          sum(is.na(var_data)),
          min(var_data, na.rm = TRUE),
          max(var_data, na.rm = TRUE),
          mean(var_data, na.rm = TRUE),
          median(var_data, na.rm = TRUE),
          as.numeric(names(sort(table(var_data), decreasing = TRUE)[1])),
          stats::sd(var_data, na.rm = TRUE),
          stats::quantile(var_data, 0.1, na.rm = TRUE),
          stats::quantile(var_data, 0.25, na.rm = TRUE),
          stats::quantile(var_data, 0.50, na.rm = TRUE),
          stats::quantile(var_data, 0.75, na.rm = TRUE),
          stats::quantile(var_data, 0.90, na.rm = TRUE)
        )
      ) |> dplyr::mutate(Valor = round(Valor, 1))

      ft <- flextable::flextable(Stats)
      # ⬇️⬇️ LÍNEA CORREGIDA: usar flextable::body_add_flextable
      doc <- flextable::body_add_flextable(doc, value = ft)

      # 3) Gráfico de densidad
      p <- ggplot2::ggplot(data.frame(x = var_data), ggplot2::aes(x = x)) +
        ggplot2::geom_density(fill = 'skyblue', color = 'blue', alpha = 0.5) +
        ggplot2::labs(title = paste("Distribución de", input$variable1))
      img <- .ggsave_tmp(p, 5, 4, 300); on.exit(unlink(img), add = TRUE)
      doc <- officer::body_add_img(doc, src = img, width = 5, height = 4)

      # 4) Guardar DOCX
      print(doc, target = file)

    }, error = function(e) {
      showNotification(paste("No se pudo generar el DOCX (Descriptivo):", conditionMessage(e)),
                       type = "error", duration = 10)
      validate(need(FALSE, "Fallo en la generación del reporte DOCX (Descriptivo)."))
    })
  }
)


  # ---- 1.6 Informe automatizado (LLM) -------------------------------
  # Guardamos el texto generado por el modelo
  p2_llm_text <- reactiveVal("")

  observeEvent(input$p2_llm_generate, {
    req(data1(), input$variable1)          # que haya datos y variable
    .need_numeric(data1(), input$variable1)

    # 1) Construir resumen numérico de la variable
    var_name  <- input$variable1
    file_name <- input$file1$name
    v         <- data1()[[var_name]]

    resumen <- paste0(
      "Archivo analizado: ", file_name, ".\n",
      "Variable: ", var_name, ".\n",
      "Casos no faltantes: ", sum(!is.na(v)), ".\n",
      "Mínimo: ", round(min(v, na.rm = TRUE), 2), ". ",
      "Máximo: ", round(max(v, na.rm = TRUE), 2), ". ",
      "Promedio: ", round(mean(v, na.rm = TRUE), 2), ". ",
      "Mediana: ", round(median(v, na.rm = TRUE), 2), ". ",
      "Desviación estándar: ", round(stats::sd(v, na.rm = TRUE), 2), ".\n",
      "Percentiles (10, 25, 50, 75, 90): ",
      paste(round(stats::quantile(v, probs = c(0.10,0.25,0.50,0.75,0.90), na.rm = TRUE), 2),
            collapse = ", "),
      "."
    )

    # 2) Contexto escrito por el usuario
    contexto_usuario <- input$p2_llm_context
    if (!nzchar(contexto_usuario)) {
      contexto_usuario <- "El usuario no proporcionó contexto adicional."
    }

    # 3) Prompt para el LLM
    prompt_llm <- paste0(
      "Eres un auditor financiero que redacta conclusiones para papeles de trabajo.\n\n",
      "Contexto general del encargo de auditoría:\n",
      contexto_usuario, "\n\n",
      "Resultados descriptivos de la variable analizada:\n",
      resumen, "\n\n",
      "Con base en esta información, redacta un párrafo claro y conciso (entre 8 y 12 líneas) ",
      "que describa los principales hallazgos del análisis descriptivo, la concentración de montos, ",
      "la presencia de valores extremos y cualquier aspecto relevante para la planificación de pruebas ",
      "de auditoría. Escribe en español, en tono técnico pero entendible, evitando viñetas."
    )

    withProgress(message = "Generando informe con LLM...", value = 0, {
      ans <- ollama_generate(prompt_llm)
      p2_llm_text(ans)
      output$p2_llm_preview <- renderText(ans)
      shinyjs::show("p2_llm_docx")   # ahora sí mostramos el botón
      incProgress(1)
    })
  })

  # ---- 1.7 Descarga del informe LLM en .docx ------------------------
  output$p2_llm_docx <- downloadHandler(
    filename = function() {
      paste0("Informe_LLM_Descriptivo_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(p2_llm_text())
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("El paquete 'officer' es necesario para generar el DOCX.")
      }

      doc <- officer::read_docx() |>
        officer::body_add_par("Informe automatizado - Análisis Descriptivo", style = "heading 1") |>
        officer::body_add_par(paste("Archivo de datos:", input$file1$name), style = "heading 2") |>
        officer::body_add_par(paste("Variable analizada:", input$variable1), style = "heading 2") |>
        officer::body_add_par("Conclusión generada con modelo de lenguaje (LLM):", style = "heading 3") |>
        officer::body_add_par(p2_llm_text(), style = "Normal")

      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  # =====================================================================
  # 2) MUESTREO MUM (p3)  - fileInput: file2
  # =====================================================================

  # ---- 2.1 Lectura + selector + alerta negativos ----------------------
  data2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    rv$data2 <- .read_any(inFile$datapath)
    rv$data2
  })

  output$variable_select_MUM <- renderUI({
    if (is.null(data2())) return(NULL)
    selectInput("variable2", "Elija una variable:", names(data2()))
  })

  observe({
    req(data2(), input$variable2)
    v <- data2()[[input$variable2]]
    rv$neg_mum <- is.numeric(v) && any(v < 0, na.rm = TRUE)
  })

  output$negativesAlertMuestreoMUM <- renderUI({
    if (isTRUE(rv$neg_mum)) {
      tags$div(class = "alert alert-danger",
               strong("¡Se detectaron valores negativos!"),
               " No es posible proceder con el muestreo MUM con montos negativos. Corrígelos para continuar.")
    }
  })
  outputOptions(output, 'negativesAlertMuestreoMUM', suspendWhenHidden = FALSE)

  # Tabla de sugerencias MUM
  sugerencias_tamaño <- data.frame(
    `Tamaño de Muestra` = c("Inferiores (<=50)", "Entre (50-100)", "Superiores (100-400)"),
    `Margen de Tolerancia (Tolerable)` = c("0.2 - 0.3", "0.03 - 0.05", "0.01 - 0.03"),
    `Error Esperado` = c("0.05 - 0.10", "0.02 - 0.05", "0.01 - 0.02"),
    `Nivel de Confianza` = c("0.90 - 0.95", "0.95 - 0.99", "> 0.99")
  )
  output$SugerenciasTamaño_MUM <- reactable::renderReactable({
    reactable::reactable(sugerencias_tamaño, bordered = TRUE, highlight = TRUE)
  })

  # Señal para conditionalPanel
  output$hasNegatives_MUM <- reactive({ isTRUE(rv$neg_mum) })
  outputOptions(output, "hasNegatives_MUM", suspendWhenHidden = FALSE)

  # ---- 2.2 Flujo principal MUM ----------------------------------------
  observeEvent(input$update_MUM, {
    if (input$freq2_MUM >= input$freq1_MUM) {
      showModal(modalDialog(title = "Advertencia",
                            "El valor 'Esperado' debe ser menor que el 'Tolerable'.",
                            easyClose = TRUE, footer = NULL))
      return(invisible())
    }
    req(data2(), input$variable2)
    .need_numeric(data2(), input$variable2)

    # 2.2.1 Tamaño de muestra
    stage1 <- planning(materiality = input$freq1_MUM,
                       expected    = input$freq2_MUM,
                       likelihood  = input$distri_1,
                       conf.level  = input$freq3_MUM)
    rv$sample_size_mum <- as.integer(stage1$n)
    output$SampleSize_MUM <- reactable::renderReactable({
      reactable::reactable(data.frame(Muestra = rv$sample_size_mum))
    })

    # 2.2.2 Semilla
    rv$seed_mum <- sample.int(100000, 1)
    output$seedvalue_MUM <- reactable::renderReactable({
      reactable::reactable(data.frame(Semilla = rv$seed_mum))
    })

    # 2.2.3 Selección PPT
datos <- data2()
total_valor <- sum(datos[[input$variable2]], na.rm = TRUE)
shiny::validate(
  shiny::need(total_valor > 0,
              "La suma de la variable es 0; imposible muestrear por PPT.")
)

    set.seed(rv$seed_mum)
    prob <- datos[[input$variable2]] / total_valor
    idx <- sample(seq_len(nrow(datos)), size = rv$sample_size_mum, replace = FALSE, prob = prob)
    rv$muestra_mum <- datos[idx, , drop = FALSE]

    output$sample_MUM <- reactable::renderReactable({
      reactable::reactable(rv$muestra_mum)
    })

    # 2.2.4 Comparación de densidades
    output$comp_dist_MUM <- highcharter::renderHighchart({
      .hc_density_compare(
        x1 = data2()[[input$variable2]],
        x2 = rv$muestra_mum[[input$variable2]],
        name1 = "Datos Originales",
        name2 = "Muestra"
      )
    })
  })

  # ---- 2.3 Descargas MUM ----------------------------------------------
  observeEvent(input$show1_MUM, {
    showModal(modalDialog(
      title = "Descargar los datos", br(),
      downloadButton("download2.1",".csv file"), br(), br(),
      downloadButton("download2.2",".txt file"), br(), br(),
      downloadButton("download2.3",".xlsx file"),
      footer = modalButton("Close"), easyClose = TRUE
    ))
  })
  output$download2.1 <- downloadHandler(
    filename = function() paste0("Muestra_MUM-", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(rv$muestra_mum, file, row.names = FALSE)
  )
  output$download2.2 <- downloadHandler(
    filename = function() paste0("Muestra_MUM-", Sys.Date(), ".txt"),
    content  = function(file) utils::write.table(rv$muestra_mum, file, row.names = FALSE)
  )
output$download2.3 <- downloadHandler(
  filename = function() paste0("Muestra_MUM-", Sys.Date(), ".xlsx"),
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  content = function(file) {
    tryCatch({
      req(rv$muestra_mum)
      df <- .sanitize_for_xlsx(rv$muestra_mum)
      openxlsx::write.xlsx(df, file)
    }, error = function(e) {
  showNotification(paste("No se pudo generar XLSX (MUM):", conditionMessage(e)),
                   type = "error", duration = 10)
  shiny::validate(
    shiny::need(FALSE, "Fallo al generar XLSX (MUM).")
  )
})
  }
)

# ---- 2.4 Reporte MUM (.docx) ----------------------------------------
generarGraficoDensidadMUM <- function(datosOriginales, datosMuestra, variable) {
  ggplot2::ggplot() +
    ggplot2::geom_density(data = datosOriginales, ggplot2::aes(x = .data[[variable]]), fill = "blue", alpha = 0.5) +
    ggplot2::geom_density(data = datosMuestra,    ggplot2::aes(x = .data[[variable]]), fill = "lightgreen", alpha = 0.5) +
    ggplot2::labs(title = "Comparación entre datos Original vs Muestra", x = variable, y = "Densidad") +
    ggplot2::theme_minimal()
}

output$downloadReport2 <- downloadHandler(
  filename = function() paste0("Muestreo_MUM_", Sys.Date(), ".docx"),
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  content = function(file) {
    tryCatch({
      # Requisitos mínimos para generar
      req(data2(), input$variable2, rv$sample_size_mum, rv$seed_mum, rv$muestra_mum)
      validate(need(nrow(rv$muestra_mum) > 0, "La muestra MUM está vacía."))

      # Paquetes de reporte (opcionalmente presentes)
      have_officer   <- requireNamespace("officer",   quietly = TRUE)
      have_flextable <- requireNamespace("flextable", quietly = TRUE)
      if (!have_officer) stop("Falta paquete 'officer'.")

      doc <- officer::read_docx() |>
        officer::body_add_par("Muestreo por Unidades Monetarias", style = "heading 1") |>
        officer::body_add_par("Parámetros", style = "heading 2") |>
        officer::body_add_par(paste("Nombre del archivo de datos:", input$file2$name), style = "Normal") |>
        officer::body_add_par(paste("Variable seleccionada:", input$variable2), style = "Normal") |>
        officer::body_add_par(paste("Error Tolerable:", input$freq1_MUM), style = "Normal") |>
        officer::body_add_par(paste("Error Esperado:", input$freq2_MUM), style = "Normal") |>
        officer::body_add_par(paste("Nivel de confianza:", input$freq3_MUM), style = "Normal") |>
        officer::body_add_par(paste("Selección de la distribución:", input$distri_1), style = "Normal") |>
        officer::body_add_par("Información de Muestreo", style = "heading 2") |>
        officer::body_add_par(paste("Tamaño de Muestra:", rv$sample_size_mum), style = "Normal") |>
        officer::body_add_par(paste("Semilla para selección por PPT:", rv$seed_mum), style = "Normal")

      # Gráfico comparativo
      g   <- generarGraficoDensidadMUM(data2(), rv$muestra_mum, input$variable2)
      img <- .ggsave_tmp(g, 7, 5, 300); on.exit(unlink(img), add = TRUE)

      doc <- doc |>
        officer::body_add_par("Gráfico comparativo entre valores originales y obtenidos por la muestra.", style = "heading 2") |>
        officer::body_add_img(src = img, width = 7, height = 5) |>
        officer::body_add_par("Muestra Seleccionada", style = "heading 2")

      # Tabla: preferir flextable si está disponible; si no, usar body_add_table sin estilo
      if (have_flextable && "body_add_flextable" %in% getNamespaceExports("flextable")) {
        ft <- flextable::flextable(rv$muestra_mum)
        doc <- flextable::body_add_flextable(doc, ft)
      } else {
        # Evita depender de un estilo que podría no existir en el template
        doc <- officer::body_add_table(doc, value = rv$muestra_mum)
      }

      print(doc, target = file)

}, error = function(e) {
  showNotification(paste("No se pudo generar el DOCX (MUM):", conditionMessage(e)),
                   type = "error", duration = 10)
  shiny::validate(
    shiny::need(FALSE, "Fallo en la generación del reporte DOCX (MUM).")
  )
})
  }
)

# ---- 2.5 Informe automatizado MUM (LLM) -----------------------------
p3_llm_text <- reactiveVal("")

observeEvent(input$p3_llm_generate, {
  # Debe existir: datos, variable, tamaño de muestra, muestra calculada
  req(data2(), input$variable2, rv$sample_size_mum, rv$muestra_mum)
  .need_numeric(data2(), input$variable2)

  v          <- data2()[[input$variable2]]
  var_name   <- input$variable2
  file_name  <- input$file2$name
  total_pop  <- sum(v, na.rm = TRUE)

  # Resumen numérico del muestreo MUM
  resumen_mum <- paste0(
    "Archivo analizado: ", file_name, ".\n",
    "Variable de muestreo: ", var_name, ".\n",
    "Importe total de la población (suma de la variable): ", round(total_pop, 2), ".\n",
    "Tamaño de la muestra MUM seleccionado: ", rv$sample_size_mum, " partidas.\n",
    "Semilla utilizada para la selección PPT: ", rv$seed_mum, ".\n",
    "Error tolerable: ", input$freq1_MUM, ".\n",
    "Error esperado: ", input$freq2_MUM, ".\n",
    "Nivel de confianza: ", input$freq3_MUM, ".\n",
    "Distribución utilizada para la planificación: ", input$distri_1, "."
  )

  # Contexto escrito por el usuario
  contexto_usuario <- input$p3_llm_context
  if (!nzchar(contexto_usuario)) {
    contexto_usuario <- "El usuario no proporcionó contexto adicional sobre el encargo de auditoría."
  }

  # Prompt específico para muestreo MUM
  prompt_mum <- paste0(
    "Eres un auditor financiero especializado en muestreo por unidades monetarias (MUM).\n\n",
    "Contexto general del encargo de auditoría:\n",
    contexto_usuario, "\n\n",
    "Resultados clave del diseño y ejecución del muestreo MUM:\n",
    resumen_mum, "\n\n",
    "Con base en esta información, redacta un párrafo claro y conciso (entre 8 y 12 líneas) ",
    "que describa los principales aspectos del muestreo: tamaño de la muestra, relación con el error ",
    "tolerable y esperado, nivel de confianza, implicaciones para el riesgo de muestreo y cómo estos ",
    "resultados orientan la planificación y extensión de las pruebas sustantivas. Escribe en español, ",
    "en tono técnico pero entendible, sin viñetas y sin repetir literalmente los datos numéricos."
  )

  withProgress(message = "Generando informe con LLM (MUM)...", value = 0, {
    ans <- ollama_generate(prompt_mum)
    p3_llm_text(ans)
    output$p3_llm_preview <- renderText(ans)
    shinyjs::show("p3_llm_docx")
    incProgress(1)
  })
})

# ---- 2.6 Descarga del informe LLM MUM (.docx) ----------------------
output$p3_llm_docx <- downloadHandler(
  filename = function() {
    paste0("Informe_LLM_MUM_", Sys.Date(), ".docx")
  },
  content = function(file) {
    req(p3_llm_text())

    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("El paquete 'officer' es necesario para generar el DOCX (MUM).")
    }

    doc <- officer::read_docx() |>
      officer::body_add_par(
        "Informe automatizado - Muestreo por Unidades Monetarias (MUM)",
        style = "heading 1"
      ) |>
      officer::body_add_par(
        paste("Archivo de datos:", input$file2$name),
        style = "heading 2"
      ) |>
      officer::body_add_par(
        paste("Variable de muestreo:", input$variable2),
        style = "heading 2"
      ) |>
      officer::body_add_par(
        "Parámetros principales del muestreo:",
        style = "heading 3"
      ) |>
      officer::body_add_par(
        paste("Error tolerable:", input$freq1_MUM),
        style = "Normal"
      ) |>
      officer::body_add_par(
        paste("Error esperado:", input$freq2_MUM),
        style = "Normal"
      ) |>
      officer::body_add_par(
        paste("Nivel de confianza:", input$freq3_MUM),
        style = "Normal"
      ) |>
      officer::body_add_par(
        paste("Distribución de planificación:", input$distri_1),
        style = "Normal"
      ) |>
      officer::body_add_par(
        paste("Tamaño de muestra MUM:", rv$sample_size_mum),
        style = "Normal"
      ) |>
      officer::body_add_par(
        paste("Semilla utilizada para PPT:", rv$seed_mum),
        style = "Normal"
      ) |>
      officer::body_add_par(
        "Conclusión generada con modelo de lenguaje (LLM):",
        style = "heading 3"
      ) |>
      officer::body_add_par(
        p3_llm_text(),
        style = "Normal"
      )

    print(doc, target = file)
  },
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
)

  # =====================================================================
  # 3) MUESTREO LES (p4)  - fileInput: file3
  # =====================================================================

  # ---- 3.1 Lectura + selector + negativos ------------------------------
  data3 <- reactive({
    inFile <- input$file3
    if (is.null(inFile)) return(NULL)
    rv$data3 <- .read_any(inFile$datapath)
    rv$data3
  })

  output$variable_select_LES <- renderUI({
    if (is.null(data3())) return(NULL)
    selectInput("variable3", "Elija una variable:", names(data3()))
  })

  observe({
    req(data3(), input$variable3)
    v <- data3()[[input$variable3]]
    rv$neg_les <- is.numeric(v) && any(v < 0, na.rm = TRUE)
  })

  output$negativesAlertMuestreoLES <- renderUI({
    if (isTRUE(rv$neg_les)) {
      tags$div(class = "alert alert-danger",
               strong("¡Se detectaron valores negativos!"),
               " No es posible proceder con el muestreo LES con montos negativos. Corrígelos para continuar.")
    }
  })
  outputOptions(output, 'negativesAlertMuestreoLES', suspendWhenHidden = FALSE)

  # Tabla de sugerencias LES
  sugerencias_tamaño_2 <- data.frame(
    `Tamaño de Muestra` = c("Inferior (<=50)", "Entre (50-100)", "Superior (100)"),
    `Margen de Tolerancia (Tolerable)` = c("0.2 - 0.3", "0.03 - 0.05", "0.01 - 0.03"),
    `Error Esperado` = c("0.05 - 0.10", "0.02 - 0.05", "0.01 - 0.02"),
    `Nivel de Confianza` = c("0.90 - 0.95", "0.95 - 0.99", "> 0.99")
  )
  output$SugerenciasTamaño_LES <- reactable::renderReactable({
    reactable::reactable(sugerencias_tamaño_2, bordered = TRUE, highlight = TRUE)
  })

  output$hasNegatives_LES <- reactive({ isTRUE(rv$neg_les) })
  outputOptions(output, "hasNegatives_LES", suspendWhenHidden = FALSE)

  # ---- 3.2 Flujo principal LES ----------------------------------------
  observeEvent(input$update_LES, {
    if (input$freq2_LES >= input$freq1_LES) {
      showModal(modalDialog(title = "Advertencia",
                            "El valor 'Esperado' debe ser menor que el 'Tolerable'.",
                            easyClose = TRUE, footer = NULL))
      return(invisible())
    }
    req(data3(), input$variable3)
    .need_numeric(data3(), input$variable3)

    # Tamaño muestra
    stage1 <- planning(materiality = input$freq1_LES,
                       expected    = input$freq2_LES,
                       likelihood  = input$distri_2,
                       conf.level  = input$freq3_LES)
    rv$sample_size_les <- as.integer(stage1$n)
    output$SampleSize_LES <- reactable::renderReactable({
      reactable::reactable(data.frame(Muestra = rv$sample_size_les))
    })

    # Semilla
    rv$seed_les <- sample.int(100000, 1)
    output$seedvalue_LES <- reactable::renderReactable({
      reactable::reactable(data.frame(Semilla = rv$seed_les))
    })

    # Selección por LES
    LES <- input$LES
    datos <- data3()
    var   <- input$variable3

    mayores <- datos[datos[[var]] > LES, , drop = FALSE]
    n_m     <- nrow(mayores)
    n_req   <- rv$sample_size_les

    if (n_m >= n_req) {
      rv$muestra_les <- head(mayores[order(-mayores[[var]]), , drop = FALSE], n_req)
    } else {
      restantes <- n_req - n_m
      menores   <- datos[datos[[var]] <= LES, , drop = FALSE]
      set.seed(rv$seed_les)
      if (nrow(menores) > 0 && restantes > 0) {
        ids <- sample.int(nrow(menores), size = min(restantes, nrow(menores)), replace = FALSE)
        rv$muestra_les <- rbind(mayores, menores[ids, , drop = FALSE])
      } else {
        rv$muestra_les <- mayores
      }
    }

    output$MuestraLES <- reactable::renderReactable({
      reactable::reactable(rv$muestra_les)
    })

    # Conteos por LES
    conteoLES <- data.frame(
      `Categoría` = c("Mayores que LES", "Menores o iguales a LES"),
      `Conteo`    = c(sum(rv$muestra_les[[var]] > LES, na.rm = TRUE),
                      sum(rv$muestra_les[[var]] <= LES, na.rm = TRUE))
    )
    output$ConteoLes <- reactable::renderReactable({
      reactable::reactable(conteoLES)
    })

    # Densidades
    output$comp_dist_LES <- highcharter::renderHighchart({
      .hc_density_compare(
        x1 = data3()[[var]],
        x2 = rv$muestra_les[[var]],
        name1 = "Datos Originales",
        name2 = "Muestra LES",
        title = "Comparación de Densidades"
      )
    })
  })

  # ---- 3.3 Descargas LES ----------------------------------------------
  observeEvent(input$show1_LES, {
    showModal(modalDialog(
      title = "Descargar los datos", br(),
      downloadButton("download4.1",".csv file"), br(), br(),
      downloadButton("download4.2",".txt file"), br(), br(),
      downloadButton("download4.3",".xlsx file"),
      footer = modalButton("Close"), easyClose = TRUE
    ))
  })
  output$download4.1 <- downloadHandler(
    filename = function() paste0("MuestraLES-", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(rv$muestra_les, file, row.names = FALSE)
  )
  output$download4.2 <- downloadHandler(
    filename = function() paste0("MuestraLES-", Sys.Date(), ".txt"),
    content  = function(file) utils::write.table(rv$muestra_les, file, row.names = FALSE)
  )

output$download4.3 <- downloadHandler(
  filename = function() paste0("MuestraLES-", Sys.Date(), ".xlsx"),
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  content = function(file) {
    tryCatch({
      req(rv$muestra_les)
      df <- .sanitize_for_xlsx(rv$muestra_les)
      openxlsx::write.xlsx(df, file)
    }, error = function(e) {
      showNotification(
        paste("No se pudo generar XLSX (LES):", conditionMessage(e)),
        type = "error", duration = 10
      )
      shiny::validate(
        shiny::need(FALSE, "Fallo al generar XLSX (LES).")
      )
    })
  }
)

# ---- 3.4 Reporte LES (.docx) ----------------------------------------
generarGraficoDensidadLES <- function(datosOriginales, datosMuestra, variable) {
  ggplot2::ggplot() +
    ggplot2::geom_density(data = datosOriginales, ggplot2::aes(x = .data[[variable]]),
                          fill = "blue", alpha = 0.5) +
    ggplot2::geom_density(data = datosMuestra, ggplot2::aes(x = .data[[variable]]),
                          fill = "lightgreen", alpha = 0.5) +
    ggplot2::labs(title = "Comparación entre datos Original vs Muestra LES",
                  x = variable, y = "Densidad") +
    ggplot2::theme_minimal()
}

# Helper: sanitizar tabla para officer::body_add_table()
.sanitize_for_docx <- function(df, max_rows = 5000) {
  if (inherits(df, "tbl_df")) df <- as.data.frame(df)

  # Aplastar list-cols y factores extraños a texto
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.list(col)) {
      df[[nm]] <- vapply(col, function(x) {
        if (length(x) == 0) return("")
        if (is.atomic(x)) paste(x, collapse = ", ")
        else as.character(paste0("<", class(x)[1], ">"))
      }, FUN.VALUE = character(1))
    } else if (inherits(col, "POSIXt")) {
      df[[nm]] <- as.character(col)
    } else if (inherits(col, "Date")) {
      df[[nm]] <- as.character(col)
    } else if (is.factor(col)) {
      df[[nm]] <- as.character(col)
    }
  }

  # Limitar filas para no generar DOCX gigantes (opcional)
  if (nrow(df) > max_rows) df <- df[seq_len(max_rows), , drop = FALSE]
  df
}

output$downloadReport3 <- downloadHandler(
  filename = function() paste0("Muestreo_LES_", Sys.Date(), ".docx"),
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  content = function(file) {
    tryCatch({
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("Falta paquete 'officer'. Instala con install.packages('officer').")
      }
      req(data3(), input$variable3, rv$sample_size_les, rv$seed_les, rv$muestra_les)

      # Documento
      doc <- officer::read_docx() |>
        officer::body_add_par("Muestreo LES", style = "heading 1") |>
        officer::body_add_par("Parámetros", style = "heading 2") |>
        officer::body_add_par(paste("Nombre del archivo de datos:", input$file3$name), style = "Normal") |>
        officer::body_add_par(paste("Variable seleccionada:", input$variable3), style = "Normal") |>
        officer::body_add_par(paste("Error Tolerable:", input$freq1_LES), style = "Normal") |>
        officer::body_add_par(paste("Error Esperado:", input$freq2_LES), style = "Normal") |>
        officer::body_add_par(paste("Nivel de confianza:", input$freq3_LES), style = "Normal") |>
        officer::body_add_par(paste("Selección de la distribución:", input$distri_2), style = "Normal") |>
        officer::body_add_par("Información de Muestreo", style = "heading 2") |>
        officer::body_add_par(paste("Tamaño de Muestra:", rv$sample_size_les), style = "Normal") |>
        officer::body_add_par(paste("Semilla para selección aleatoria inferior al LES:", rv$seed_les), style = "Normal")

      # Gráfico
      g <- generarGraficoDensidadLES(data3(), rv$muestra_les, input$variable3)
      img <- .ggsave_tmp(g, 7, 5, 300)
      on.exit(unlink(img), add = TRUE)

      doc <- doc |>
        officer::body_add_par("Gráfico comparativo entre valores originales y obtenidos por la muestra.", style = "heading 2") |>
        officer::body_add_img(src = img, width = 7, height = 5)

      # Tabla (sanitizada)
      tabla_saneada <- .sanitize_for_docx(rv$muestra_les)
      doc <- doc |>
        officer::body_add_par("Muestra Seleccionada", style = "heading 2") |>
        officer::body_add_table(tabla_saneada, style = "table_template")

      print(doc, target = file)
    }, error = function(e) {
      showNotification(
        paste("No se pudo generar el DOCX (LES):", conditionMessage(e)),
        type = "error", duration = 10
      )
      shiny::validate(
        shiny::need(FALSE, "Fallo en la generación del reporte DOCX (LES).")
      )
    })
  }
)

# =====================================================================
# 3.5 Informe automatizado LES (LLM)
# =====================================================================

# Texto generado por el modelo para LES
p4_llm_text <- reactiveVal("")

observeEvent(input$p4_llm_generate, {
  # Necesitamos datos, variable numérica y una muestra ya calculada
  req(data3(), input$variable3, rv$sample_size_les, rv$muestra_les)
  .need_numeric(data3(), input$variable3)

  var_name    <- input$variable3
  file_name   <- input$file3$name
  les_value   <- input$LES
  v_poblacion <- data3()[[var_name]]
  v_muestra   <- rv$muestra_les[[var_name]]

  # Resumen cuantitativo para alimentar al LLM
  resumen <- paste0(
    "Archivo analizado: ", file_name, ".\n",
    "Variable monetaria utilizada para el muestreo: ", var_name, ".\n",
    "Número total de partidas en la población: ", sum(!is.na(v_poblacion)), ".\n",
    "Importe total de la población (suma de la variable): ",
    round(sum(v_poblacion, na.rm = TRUE), 2), ".\n",
    "Tamaño de muestra obtenido: ", rv$sample_size_les, " partidas.\n",
    "Valor LES utilizado: ", les_value, ".\n",
    "Parámetros del plan de muestreo: \n",
    "  - Error tolerable: ", input$freq1_LES, "\n",
    "  - Error esperado: ", input$freq2_LES, "\n",
    "  - Nivel de confianza: ", input$freq3_LES, "\n",
    "  - Distribución asumida: ", input$distri_2, "\n",
    "En la muestra LES se obtuvieron ", sum(v_muestra > les_value, na.rm = TRUE),
    " partidas por encima del LES y ",
    sum(v_muestra <= les_value, na.rm = TRUE),
    " por debajo o iguales al LES.\n"
  )

  # Contexto que escribe el auditor
  contexto_usuario <- input$p4_llm_context
  if (!nzchar(contexto_usuario)) {
    contexto_usuario <- "El usuario no proporcionó contexto adicional."
  }

  # Prompt completo para el modelo
  prompt_llm <- paste0(
    "Eres un auditor financiero especializado en muestreos de detalle ",
    "mediante el método LES (Límite Específico de Selección).\n\n",
    "Contexto general del encargo de auditoría:\n",
    contexto_usuario, "\n\n",
    "Resumen del diseño y resultados del muestreo LES:\n",
    resumen, "\n",
    "Con base en esta información, redacta un párrafo claro y conciso ",
    "(entre 8 y 12 líneas) que describa:\n",
    "- Cómo se diseñó el muestreo LES (parámetros clave y uso del umbral LES).\n",
    "- Qué revelan los resultados sobre la concentración de montos y los saldos ",
    "por encima del umbral.\n",
    "- Implicaciones para la planificación de pruebas sustantivas y la evaluación ",
    "del riesgo de incorrección material.\n\n",
    "Escribe en español, en tono técnico pero entendible, sin viñetas ni listas."
  )

  withProgress(message = "Generando informe con LLM...", value = 0, {
    # 🔁 Igual que en Descriptivo y MUM: usamos la MISMA función helper
    ans <- ollama_generate(prompt_llm)

    p4_llm_text(ans)
    output$p4_llm_preview <- renderText(ans)
    shinyjs::show("p4_llm_docx")

    incProgress(1)
  })
})

# Descarga del informe LES generado por LLM en DOCX
output$p4_llm_docx <- downloadHandler(
  filename = function() {
    paste0("Informe_LLM_Muestreo_LES_", Sys.Date(), ".docx")
  },
  content = function(file) {
    tryCatch({
      req(p4_llm_text())
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("El paquete 'officer' es necesario para generar el DOCX.")
      }

      doc <- officer::read_docx() |>
        officer::body_add_par(
          "Informe automatizado - Muestreo LES",
          style = "heading 1"
        ) |>
        officer::body_add_par(
          paste("Archivo de datos:", input$file3$name),
          style = "heading 2"
        ) |>
        officer::body_add_par(
          paste("Variable utilizada en el muestreo:", input$variable3),
          style = "heading 2"
        ) |>
        officer::body_add_par(
          paste("Valor LES aplicado:", input$LES),
          style = "Normal"
        ) |>
        officer::body_add_par(
          "Conclusión generada con modelo de lenguaje (LLM):",
          style = "heading 3"
        ) |>
        officer::body_add_par(p4_llm_text(), style = "Normal")

      print(doc, target = file)
    }, error = function(e) {
      showNotification(
        paste("No se pudo generar el DOCX (Informe LLM LES):", conditionMessage(e)),
        type = "error", duration = 10
      )
      shiny::validate(
        shiny::need(FALSE, "Fallo en la generación del informe LLM (LES).")
      )
    })
  },
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
)

  # =====================================================================
  # 4) MUESTREO POR ATRIBUTOS (p5)  - fileInput: file4
  # =====================================================================

  # ---- 4.1 Lectura + selector -----------------------------------------
  data4 <- reactive({
    inFile <- input$file4
    if (is.null(inFile)) return(NULL)
    rv$data4 <- .read_any(inFile$datapath)
    rv$data4
  })

  output$variable_select_Atri <- renderUI({
    if (is.null(data4())) return(NULL)
    selectInput("variable4", "Elija una variable:", names(data4()))
  })

  # Tabla de sugerencias
  sugerencias_tamaño_3 <- data.frame(
    `Tamaño de Muestra` = c("Inferior (<=50)", "Entre (50-100)", "Superior (100)"),
    `Margen de Tolerancia (Tolerable)` = c("0.2 - 0.3", "0.03 - 0.05", "0.01 - 0.03"),
    `Error Esperado` = c("0.05 - 0.10", "0.02 - 0.05", "0.01 - 0.02"),
    `Nivel de Confianza` = c("0.90 - 0.95", "0.95 - 0.99", "> 0.99")
  )
  output$SugerenciasTamaño_Atri <- reactable::renderReactable({
    reactable::reactable(sugerencias_tamaño_3, bordered = TRUE, highlight = TRUE)
  })

  # ---- 4.2 Flujo Atributos --------------------------------------------
  observeEvent(input$update_Atri, {
    req(data4(), input$variable4)

    # Tamaño muestra
    stage1 <- planning(materiality = input$freq1_Atri,
                       expected    = input$freq2_Atri,
                       likelihood  = input$distri_3,
                       conf.level  = input$freq3_Atri)
    rv$sample_size_atri <- as.integer(stage1$n)
    output$SampleSize_Atri <- reactable::renderReactable({
      reactable::reactable(data.frame(Muestra = rv$sample_size_atri))
    })

    # Semilla
    rv$seed_atri <- sample.int(100000, 1)
    output$seedvalue_Atri <- reactable::renderReactable({
      reactable::reactable(data.frame(Semilla = rv$seed_atri))
    })

    # Selección aleatoria simple
    set.seed(rv$seed_atri)
    n <- min(rv$sample_size_atri, nrow(data4()))
    rv$muestra_atri <- dplyr::sample_n(data4(), size = n)
    output$tablaMuestraAtri <- reactable::renderReactable({
      reactable::reactable(rv$muestra_atri)
    })

    # Tablas de porcentajes (origen vs muestra)
    tablaOrigenPorce <- reactive({
      req(data4(), input$variable4)
      data4() |>
        dplyr::group_by(Categoria = .data[[input$variable4]]) |>
        dplyr::tally(name = "Total") |>
        dplyr::mutate(Porcentaje = round((Total / sum(Total)) * 100, 1)) |>
        dplyr::ungroup()
    })

    tablaMuestraPorce <- reactive({
      req(rv$muestra_atri, input$variable4)
      rv$muestra_atri |>
        dplyr::group_by(Categoria = .data[[input$variable4]]) |>
        dplyr::tally(name = "Total") |>
        dplyr::mutate(Porcentaje = round((Total / sum(Total)) * 100, 1)) |>
        dplyr::ungroup()
    })

    # Gráfico comparativo porcentual
    output$graficoComparativo2 <- highcharter::renderHighchart({
      req(tablaOrigenPorce(), tablaMuestraPorce())
      origen  <- tablaOrigenPorce()
      muestra <- tablaMuestraPorce()
      comb <- merge(origen, muestra, by = "Categoria", all = TRUE)
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "bar") %>%
        highcharter::hc_title(text = "Comparación de Porcentajes por Categoría") %>%
        highcharter::hc_xAxis(categories = comb$Categoria) %>%
        highcharter::hc_yAxis(title = list(text = "Porcentaje")) %>%
        highcharter::hc_add_series(name = "Original", data = comb$Porcentaje.x) %>%
        highcharter::hc_add_series(name = "Muestra",  data = comb$Porcentaje.y) %>%
        highcharter::hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
        highcharter::hc_tooltip(shared = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%</b><br/>') %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_exporting(enabled = TRUE)
    })

    # Descargas
    observeEvent(input$show1_Atri, {
      showModal(modalDialog(
        title = "Descargar los datos", br(),
        downloadButton("download5.1",".csv file"), br(), br(),
        downloadButton("download5.2",".txt file"), br(), br(),
        downloadButton("download5.3",".xlsx file"),
        footer = modalButton("Close"), easyClose = TRUE
      ))
    })
    output$download5.1 <- downloadHandler(
      filename = function() paste0("MuestraAtributo-", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(rv$muestra_atri, file, row.names = FALSE)
    )
    output$download5.2 <- downloadHandler(
      filename = function() paste0("MuestraAtributo-", Sys.Date(), ".txt"),
      content  = function(file) utils::write.table(rv$muestra_atri, file, row.names = FALSE)
    )
output$download5.3 <- downloadHandler(
  filename = function() paste0("MuestraAtributo-", Sys.Date(), ".xlsx"),
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  content = function(file) {
    tryCatch({
      req(rv$muestra_atri)
      df <- .sanitize_for_xlsx(rv$muestra_atri)
      openxlsx::write.xlsx(df, file)
    }, error = function(e) {
      showNotification(paste("No se pudo generar XLSX (Atributos):", conditionMessage(e)),
                       type = "error", duration = 10)
      validate(need(FALSE, "Fallo al generar XLSX (Atributos)."))
    })
  }
)

    # Reporte atributos
    generarGraficoPorcentajesAtri <- function(datosOriginales, datosMuestra) {
      ggplot2::ggplot() +
        ggplot2::geom_bar(data = datosOriginales, ggplot2::aes(x = Categoria, y = Porcentaje, fill = "Original"),
                          stat = "identity", position = ggplot2::position_dodge(width = 0.8), width = 0.35) +
        ggplot2::geom_bar(data = datosMuestra, ggplot2::aes(x = Categoria, y = Porcentaje, fill = "Muestra"),
                          stat = "identity", position = ggplot2::position_dodge(width = 0.8), width = 0.35) +
        ggplot2::scale_fill_manual(values = c("Original" = "lightblue", "Muestra" = "lightgreen")) +
        ggplot2::labs(title = "Comparación de Porcentajes por Categoría", x = "Categoría", y = "Porcentaje") +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip() +
        ggplot2::theme(legend.position = "bottom")
    }

    output$downloadReport4 <- downloadHandler(
      filename = function() paste0("Muestreo_Atributos_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        tryCatch({
          if (!requireNamespace("officer", quietly = TRUE)) stop("Falta paquete 'officer'.")
          req(data4(), input$variable4, rv$sample_size_atri, rv$seed_atri, rv$muestra_atri)

          doc <- officer::read_docx() |>
            officer::body_add_par("Muestreo Atributos", style = "heading 1") |>
            officer::body_add_par("Parámetros", style = "heading 2") |>
            officer::body_add_par(paste("Nombre del archivo de datos:", input$file4$name), style = "Normal") |>
            officer::body_add_par(paste("Variable seleccionada:", input$variable4), style = "Normal") |>
            officer::body_add_par(paste("Error Tolerable:", input$freq1_Atri), style = "Normal") |>
            officer::body_add_par(paste("Error Esperado:", input$freq2_Atri), style = "Normal") |>
            officer::body_add_par(paste("Nivel de confianza:", input$freq3_Atri), style = "Normal") |>
            officer::body_add_par(paste("Selección de la distribución:", input$distri_3), style = "Normal") |>
            officer::body_add_par("Información de Muestreo", style = "heading 2") |>
            officer::body_add_par(paste("Tamaño de Muestra:", rv$sample_size_atri), style = "Normal") |>
            officer::body_add_par(paste("Semilla para selección aleatoria:", rv$seed_atri), style = "Normal")

          datosOrigen  <- tablaOrigenPorce()
          datosMuestra <- tablaMuestraPorce()
          g <- generarGraficoPorcentajesAtri(datosOrigen, datosMuestra)
          img <- .ggsave_tmp(g, 8, 6, 300); on.exit(unlink(img), add = TRUE)
          doc <- doc |>
            officer::body_add_par("Gráfico comparativo entre valores originales y obtenidos por la muestra.", style = "heading 2") |>
            officer::body_add_img(src = img, width = 8, height = 6) |>
            officer::body_add_par("Muestra Seleccionada", style = "heading 2") |>
            officer::body_add_table(value = rv$muestra_atri, style = "table_template")

          print(doc, target = file)
        }, error = function(e) {
          showNotification(paste("No se pudo generar el DOCX (Atributos):", conditionMessage(e)), type = "error", duration = 10)
          validate(need(FALSE, "Fallo en la generación del reporte DOCX (Atributos)."))
        })
      }
    )
  }) # /observeEvent(update_Atri)

  # =====================================================================
  # 5) EVALUACIÓN (p6)  - fileInput: file5
  # =====================================================================

  # ---- 5.1 Lectura + selectors ----------------------------------------
  data5 <- reactive({
    inFile <- input$file5
    if (is.null(inFile)) return(NULL)
    rv$data5 <- .read_any(inFile$datapath)
    rv$data5
  })

  output$var1 <- renderUI({
    req(data5())
    selectInput("select_var1", "Seleccione Variable 1: Observado", names(data5()))
  })
  output$var2 <- renderUI({
    req(data5())
    selectInput("select_var2", "Seleccione Variable 2: Auditado", names(data5()))
  })

  DatosEval <- reactive({
    req(data5(), input$select_var1, input$select_var2)
    data5() |>
      dplyr::rename(Observado = !!input$select_var1, Auditado = !!input$select_var2)
  })

  Diferencias <- reactive({
    req(DatosEval())
    DatosEval() |>
      dplyr::mutate(Diferencia = abs(Observado - Auditado)) |>
      dplyr::filter(Diferencia != 0) |>
      dplyr::arrange(dplyr::desc(Diferencia))
  })

  # ---- 5.2 Acciones al presionar "Evaluación" --------------------------
  observeEvent(input$analizar, {

    # Tabla base
    output$Tabla2 <- reactable::renderReactable({
      reactable::reactable(DatosEval())
    })

    # Scatter Observado vs Auditado
    output$ScatterPlot <- highcharter::renderHighchart({
      req(DatosEval())
      dd <- DatosEval()
      highcharter::hchart(dd, "scatter", highcharter::hcaes(x = Observado, y = Auditado)) %>%
        highcharter::hc_add_series(
          data = highcharter::list_parse(data.frame(x = c(0, max(dd$Observado, na.rm = TRUE)),
                                                    y = c(0, max(dd$Observado, na.rm = TRUE)))),
          type = "line", name = "y = x"
        ) %>%
        highcharter::hc_chart(zoomType = "xy") %>%
        highcharter::hc_exporting(enabled = TRUE)
    })

    # Tabla de diferencias
    output$Tabla3 <- reactable::renderReactable({
      reactable::reactable(Diferencias())
    })

    # Indicadores riesgo
    IndicadoresRiesgo <- function(datos) {
      suma_obs <- round(sum(datos$Observado, na.rm = TRUE), 1)
      suma_aud <- round(sum(datos$Auditado, na.rm = TRUE), 1)
      n_obs <- nrow(datos); n_aud <- nrow(datos)
      promedio_obs <- round(mean(datos$Observado, na.rm = TRUE), 1)
      promedio_aud <- round(mean(datos$Auditado, na.rm = TRUE), 1)
      conteo_dif <- sum(datos$Observado != datos$Auditado, na.rm = TRUE)
      sobrev <- sum(datos$Observado > datos$Auditado, na.rm = TRUE)
      infrav <- sum(datos$Observado < datos$Auditado, na.rm = TRUE)
      suma_sobrev <- round(sum(datos$Observado[datos$Observado > datos$Auditado] - datos$Auditado[datos$Observado > datos$Auditado], na.rm = TRUE), 1)
      suma_infrav <- round(sum(datos$Auditado[datos$Observado < datos$Auditado] - datos$Observado[datos$Observado < datos$Auditado], na.rm = TRUE), 1)
      dif_total <- round(sum(abs(datos$Observado - datos$Auditado), na.rm = TRUE), 1)
      porcentaje_dif <- round((dif_total / max(suma_aud, 1e-9)) * 100, 1)

      data.frame(
        Indicador = c("Suma total Observados","Suma total Auditados","n Observados","n Auditados",
                      "Monto promedio Observado","Monto promedio Auditado","Conteo Observados vs Auditado",
                      "Cantidad de sobrevaloraciones","Cantidad de infravaloraciones",
                      "Diferencia total Observados y Auditados","Suma de sobrevaloraciones",
                      "Suma de infravaloraciones","Porcentaje de diferencia"),
        Valor = c(suma_obs, suma_aud, n_obs, n_aud, promedio_obs, promedio_aud, conteo_dif,
                  sobrev, infrav, dif_total, suma_sobrev, suma_infrav, porcentaje_dif)
      )
    }

    output$Riesgo <- reactable::renderReactable({
      reactable::reactable(IndicadoresRiesgo(DatosEval()))
    })

    # Scatter con límites de confianza (bandas aprox.)
    output$ScatterPlot_limit <- highcharter::renderHighchart({
      req(DatosEval())
      dd <- DatosEval()
      std_dev <- stats::sd(dd$Observado - dd$Auditado, na.rm = TRUE)
      lim_inf <- -1.96 * std_dev
      lim_sup <-  1.96 * std_dev
      xmax <- max(dd$Observado, na.rm = TRUE)

      highcharter::hchart(dd, 'scatter', highcharter::hcaes(x = Observado, y = Auditado)) %>%
        highcharter::hc_add_series(data = highcharter::list_parse(data.frame(x = c(0, xmax), y = c(0, xmax))),
                                   type = 'line', name = 'y = x') %>%
        highcharter::hc_add_series(data = highcharter::list_parse(data.frame(x = c(0, xmax), y = c(lim_inf, lim_inf + xmax))),
                                   type = 'line', name = 'Límite Inferior', color = "blue") %>%
        highcharter::hc_add_series(data = highcharter::list_parse(data.frame(x = c(0, xmax), y = c(lim_sup, lim_sup + xmax))),
                                   type = 'line', name = 'Límite Superior', color = "blue") %>%
        highcharter::hc_chart(zoomType = "xy") %>%
        highcharter::hc_exporting(enabled = TRUE)
    })

    # Botón “Descargar diferencias”
    observeEvent(input$show2, {
      showModal(modalDialog(
        title = "Descargar las diferencias", br(),
        downloadButton("download3.1",".csv file"), br(), br(),
        downloadButton("download3.2",".txt file"), br(), br(),
        downloadButton("download3.3",".xlsx file"),
        footer = modalButton("Close"), easyClose = TRUE
      ))
    })
    output$download3.1 <- downloadHandler(
      filename = function() paste0("Diferencias-", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(Diferencias(), file, row.names = FALSE)
    )
    output$download3.2 <- downloadHandler(
      filename = function() paste0("Diferencias-", Sys.Date(), ".txt"),
      content  = function(file) utils::write.table(Diferencias(), file, row.names = FALSE)
    )
output$download3.3 <- downloadHandler(
  filename = function() paste0("Diferencias-", Sys.Date(), ".xlsx"),
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  content = function(file) {
    tryCatch({
      req(Diferencias())
      df <- .sanitize_for_xlsx(Diferencias())
      openxlsx::write.xlsx(df, file)
    }, error = function(e) {
      showNotification(paste("No se pudo generar XLSX (Diferencias):", conditionMessage(e)),
                       type = "error", duration = 10)
      validate(need(FALSE, "Fallo al generar XLSX (Diferencias)."))
    })
  }
)

    # Umbrales y tabla de decisión
    calculaIndicadoresDecision <- function(datos, monto_maximo, porcentaje_umbral, conteo_umbral, casos_umbral) {
      stopifnot(all(c("Observado","Auditado") %in% names(datos)))
      monto_dif_tot <- round(sum(abs(datos$Observado - datos$Auditado), na.rm = TRUE), 1)
      porcentaje_dif <- round((monto_dif_tot / max(sum(datos$Auditado, na.rm = TRUE), 1e-9)) * 100, 1)
      conteo_dif <- sum(datos$Observado != datos$Auditado, na.rm = TRUE)
      std_dev <- stats::sd(datos$Observado - datos$Auditado, na.rm = TRUE)
      lim_inf <- -1.96 * std_dev; lim_sup <- 1.96 * std_dev
      casos_fuera <- sum((datos$Observado - datos$Auditado) < lim_inf | (datos$Observado - datos$Auditado) > lim_sup, na.rm = TRUE)

      valores   <- c(monto_dif_tot, porcentaje_dif, conteo_dif, casos_fuera)
      umbrales  <- c(monto_maximo, porcentaje_umbral, conteo_umbral, casos_umbral)
      decision  <- ifelse(valores <= umbrales, "Aceptable", "No Aceptable")
      data.frame(
        Indicador = c("Monto Diferencia Total", "Porcentaje de Diferencia", "Conteo Diferencias", "Casos Fuera de Límites"),
        Valor     = valores,
        Umbral    = umbrales,
        Decision  = decision
      )
    }

    observeEvent(input$auditEval, {
      req(DatosEval())
      mmax <- as.numeric(input$monto_maximo)
      pmax <- as.numeric(input$porcentaje_umbral)
      cmax <- as.numeric(input$conteo_umbral)
      cfux <- as.numeric(input$casos_umbral)

      rv$eval_decision <- calculaIndicadoresDecision(DatosEval(), mmax, pmax, cmax, cfux)
      output$Eval <- reactable::renderReactable({
        reactable::reactable(rv$eval_decision)
      })
    })
  })

  # ---- 5.3 Reporte Evaluación (.docx) ---------------------------------
  EvalScatterPlot <- function(datos) {
    ggplot2::ggplot(datos, ggplot2::aes(x = Observado, y = Auditado)) +
      ggplot2::geom_point(color = "blue") +
      ggplot2::geom_smooth(method = "lm", color = "red") +
      ggplot2::labs(title = "Gráfico de dispersión Observado vs Auditado",
                    x = "Observado", y = "Auditado") +
      ggplot2::theme_minimal()
  }

  ScatterPlotLim <- function(datos) {
    stopifnot(all(c("Observado","Auditado") %in% names(datos)))
    dif <- datos$Observado - datos$Auditado
    std_dev <- stats::sd(dif, na.rm = TRUE)
    mean_diff <- mean(dif, na.rm = TRUE)

    intercept <- 0; slope <- 1
    intercept_inf <- intercept + (mean_diff - 1.96 * std_dev)
    intercept_sup <- intercept + (mean_diff + 1.96 * std_dev)

    ggplot2::ggplot(datos, ggplot2::aes(x = Observado, y = Auditado)) +
      ggplot2::geom_point(color = "blue") +
      ggplot2::geom_abline(intercept = intercept,     slope = slope, linetype = "dashed", color = "red") +
      ggplot2::geom_abline(intercept = intercept_inf, slope = slope, linetype = "dashed", color = "darkgreen") +
      ggplot2::geom_abline(intercept = intercept_sup, slope = slope, linetype = "dashed", color = "darkgreen") +
      ggplot2::labs(title = "Scatter Plot con Límites de Confianza", x = "Observado", y = "Auditado") +
      ggplot2::theme_minimal()
  }

  output$downloadReport5 <- downloadHandler(
    filename = function() paste0("Evaluacion_Auditoria_", Sys.Date(), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content  = function(file) {
      tryCatch({
        if (!requireNamespace("officer", quietly = TRUE)) stop("Falta paquete 'officer'.")
        req(data5(), input$select_var1, input$select_var2, DatosEval())

        doc <- officer::read_docx() |>
          officer::body_add_par("Evaluación", style = "heading 1") |>
          officer::body_add_par("Parámetros", style = "heading 2") |>
          officer::body_add_par(paste("Nombre del archivo:", input$file5$name), style = "Normal") |>
          officer::body_add_par(paste("Variable Observada:", input$select_var1), style = "Normal") |>
          officer::body_add_par(paste("Variable Auditada:",  input$select_var2), style = "Normal") |>
          officer::body_add_par("1. Análisis de Diferencias", style = "heading 2")

        if (!is.null(Diferencias()) && nrow(Diferencias()) > 0) {
          doc <- doc |> officer::body_add_table(Diferencias(), style = "table_template")
        }

        p1 <- EvalScatterPlot(DatosEval()); img1 <- .ggsave_tmp(p1, 7, 5, 300); on.exit(unlink(img1), add = TRUE)
        doc <- doc |>
          officer::body_add_par("Gráfico de dispersión Observado vs Auditado", style = "heading 2") |>
          officer::body_add_img(src = img1, width = 7, height = 5)

        IndicadoresRiesgo <- function(datos) {
          suma_obs <- round(sum(datos$Observado, na.rm = TRUE), 1)
          suma_aud <- round(sum(datos$Auditado, na.rm = TRUE), 1)
          n_obs <- nrow(datos); n_aud <- nrow(datos)
          promedio_obs <- round(mean(datos$Observado, na.rm = TRUE), 1)
          promedio_aud <- round(mean(datos$Auditado, na.rm = TRUE), 1)
          conteo_dif <- sum(datos$Observado != datos$Auditado, na.rm = TRUE)
          sobrev <- sum(datos$Observado > datos$Auditado, na.rm = TRUE)
          infrav <- sum(datos$Observado < datos$Auditado, na.rm = TRUE)
          suma_sobrev <- round(sum(datos$Observado[datos$Observado > datos$Auditado] - datos$Auditado[datos$Observado > datos$Auditado], na.rm = TRUE), 1)
          suma_infrav <- round(sum(datos$Auditado[datos$Observado < datos$Auditado] - datos$Observado[datos$Observado < datos$Auditado], na.rm = TRUE), 1)
          dif_total <- round(sum(abs(datos$Observado - datos$Auditado), na.rm = TRUE), 1)
          porcentaje_dif <- round((dif_total / max(suma_aud, 1e-9)) * 100, 1)
          data.frame(
            Indicador = c("Suma total Observados","Suma total Auditados","n Observados","n Auditados",
                          "Monto promedio Observado","Monto promedio Auditado","Conteo Observados vs Auditado",
                          "Cantidad de sobrevaloraciones","Cantidad de infravaloraciones",
                          "Diferencia total Observados y Auditados","Suma de sobrevaloraciones",
                          "Suma de infravaloraciones","Porcentaje de diferencia"),
            Valor = c(suma_obs, suma_aud, n_obs, n_aud, promedio_obs, promedio_aud, conteo_dif,
                      sobrev, infrav, dif_total, suma_sobrev, suma_infrav, porcentaje_dif)
          )
        }
        doc <- doc |>
          officer::body_add_par("3. Indicadores de Riesgo", style = "heading 2") |>
          officer::body_add_table(IndicadoresRiesgo(DatosEval()), style = "table_template")

        p2 <- ScatterPlotLim(DatosEval()); img2 <- .ggsave_tmp(p2, 7, 5, 300); on.exit(unlink(img2), add = TRUE)
        doc <- doc |>
          officer::body_add_par("Scatter Plot con Límites de Confianza", style = "heading 2") |>
          officer::body_add_img(src = img2, width = 7, height = 5)

        if (!is.null(rv$eval_decision)) {
          doc <- doc |>
            officer::body_add_par("4. Criterio empírico de la evaluación de la auditoría", style = "heading 2") |>
            officer::body_add_table(rv$eval_decision, style = "table_template")
        }

        print(doc, target = file)
      }, error = function(e) {
        showNotification(paste("No se pudo generar el DOCX (Evaluación):", conditionMessage(e)), type = "error", duration = 10)
        validate(need(FALSE, "Fallo en la generación del reporte DOCX (Evaluación)."))
      })
    }
  )
}