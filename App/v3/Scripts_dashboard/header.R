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
  titleWidth = 260,
  
  # ---- TÍTULO ----
  title = tagList(
    tags$span(
      icon("chart-bar"),
      tags$span("Muestreo Auditoría",
                style = "font-weight:600; margin-left:6px;")
    )
  )
)

# ============================================================
# INSERTAMOS LOS BOTONES EN EL ORDEN CORRECTO
# ============================================================

# 1. Welcome Guide — debe ir DEBAJO DEL MENÚ (a su derecha)
welcome_btn <- tags$li(
  class = "dropdown",
  style = "padding:8px 10px;",
  actionButton(
    inputId = "welcome_guide",
    label = tagList(icon("info-circle"), "Welcome Guide"),
    class = "btn btn-success btn-sm"
  )
)

# 2. Switch Día/Noche — extremo derecho
darkmode_btn <- tags$li(
  class = "dropdown",
  style = "padding:12px 20px 0 10px;",
  shinyWidgets::materialSwitch(
    inputId = "dark_mode",
    label = NULL,
    value = FALSE,
    status = "primary",
    right = TRUE
  )
)

# ------------------------------------------------------------
# REORDENAMOS EL HEADER de forma segura
# ------------------------------------------------------------
# children[[2]] es donde Shinydashboard guarda los items extra
app_header$children[[2]] <- tagList(
  welcome_btn,     # ✔ justo después del menú
  darkmode_btn     # ✔ extremo derecho
)

header <- app_header