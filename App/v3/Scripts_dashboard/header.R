###############################################################
# 🧭 Encabezado (Header) del Dashboard
# -------------------------------------------------------------

app_header <- shinydashboard::dashboardHeader(
  titleWidth = 260,

  # --- Título del dashboard ---
  title = tags$span(
    icon("chart-bar"),
    tags$span("Muestreo Auditoría",
              style = "font-weight:600; margin-left:6px;")
  ),

  # --- Contenedor flex para Welcome Guide + Switch ---
  tags$li(
    class = "dropdown",
    style = "
      width: 100%;
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding-right: 20px;
    ",

    # SUBCONTENEDOR IZQUIERDO → botón de tres rayas ya está ahí,
    # así que colocamos Welcome Guide justo después
    tags$div(
      style = "display:flex; align-items:center; gap:10px; margin-left: 10px;",
      actionButton(
        inputId = "welcome_guide",
        label = tagList(
          icon("info-circle"),
          tags$span("Welcome Guide")
        ),
        class = "btn btn-success btn-sm"
      )
    ),

    # SUBCONTENEDOR DERECHO → switch modo oscuro
    tags$div(
      style = "display:flex; align-items:center;",
      shinyWidgets::materialSwitch(
        inputId = "dark_mode",
        label = NULL,
        value = FALSE,
        status = "primary",
        right = TRUE
      )
    )
  )
)

header <- app_header