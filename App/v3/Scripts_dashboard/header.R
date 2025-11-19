###############################################################
# 🧭 Encabezado (Header) del Dashboard
# -------------------------------------------------------------
# Este script define la barra superior de la aplicación Shiny,
# visible en todas las pestañas del dashboard.
#
# Contiene:
#   • Título principal del sistema.
#   • (Opcional) espacio para menús, iconos o notificaciones.
#
# NOTA:
#   Este objeto se pasa directamente al argumento `header`
#   dentro de `dashboardPage()` en el archivo ui.R.
###############################################################

# =============================================================
# (1) Definición básica del encabezado
# -------------------------------------------------------------
# dashboardHeader() proviene del paquete {shinydashboard}.
# Acepta parámetros como:
#   - title: texto o HTML (por ejemplo, iconos o logotipos)
#   - titleWidth: ancho fijo del título (opcional)
#   - tags$li(...): permite insertar elementos personalizados.
# =============================================================

app_header <- shinydashboard::dashboardHeader(
  titleWidth = 320,
  
  # ==========================================================
  #  TÍTULO + BOTÓN "WELCOME GUIDE"
  #  (aparecen inmediatamente a la derecha del botón ≡)
  # ==========================================================
  title = tags$div(
    style = "display:flex; align-items:center;",
    
    # Botón Welcome Guide
    actionButton(
      inputId = "welcome_guide",
      label   = tagList(icon("info-circle"), "Welcome Guide"),
      class   = "btn btn-success btn-sm",
      style   = "margin-right:12px;"
    ),
    
    # Icono + texto del título
    tags$span(
      icon("chart-bar"),
      style = "margin-right:6px;"
    ),
    tags$span(
      "Muestreo Auditoría",
      style = "font-weight:600;"
    )
  ),
  
  # ==========================================================
  #  SWITCH DÍA / NOCHE (EXTREMO DERECHO)
  # ==========================================================
  tags$li(
    class = "dropdown",
    style = "padding:12px 20px 0 10px;",
    shinyWidgets::materialSwitch(
      inputId = "dark_mode",
      label   = NULL,
      value   = FALSE,
      status  = "primary",
      right   = TRUE
    )
  )
)

# Alias para compatibilidad
header <- app_header