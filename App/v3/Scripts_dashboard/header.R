###############################################################
# 🧭 Encabezado (Header) del Dashboard
# -------------------------------------------------------------
app_header <- shinydashboard::dashboardHeader(
  titleWidth = 260,
  title = tagList(
    tags$span(
      icon("chart-bar"),
      tags$span("Muestreo Auditoría",
                style = "font-weight:600; margin-left:6px;")
    )
  ),
  
  # --- Elementos del lado IZQUIERDO (después del toggle sidebar) ---
  tags$li(
    class = "dropdown",
    style = "position: absolute; left: 270px; top: 0; padding: 8px 10px;",
    actionButton(
      inputId = "welcome_guide",
      label = tagList(
        icon("info-circle"),
        tags$span("Welcome Guide", style = "margin-left:4px;")
      ),
      class = "btn btn-success btn-sm"
    )
  ),
  
  # --- Switch Day / Night (EXTREMO DERECHO) ---
  tags$li(
    class = "dropdown",
    style = "padding: 12px 10px 0 10px;",
    shinyWidgets::materialSwitch(
      inputId = "dark_mode",
      label = NULL,
      value = FALSE,
      status = "primary",
      right = TRUE
    )
  )
)

header <- app_header