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
  title = tagList(
    # Icono + texto
    tags$span(
      icon("chart-bar"),
      tags$span("Muestreo Auditoría",
                style = "font-weight:600; margin-left:6px;")
    )
  ),
  # --- Botón Welcome Guide (lado derecho) ---
  tags$li(
    class = "dropdown",
    style = "padding:8px 10px;",
    actionButton(
      inputId = "welcome_guide",
      label = tagList(
        icon("info-circle"),
        tags$span("Welcome Guide")
      ),
      class = "btn btn-success btn-sm"
    )
  ),
  # --- Switch Day / Night (lado derecho) ---
  tags$li(
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
)
# =============================================================
# (2) Alias para compatibilidad retroactiva
# -------------------------------------------------------------
# Si el código anterior hacía referencia a `header` (sin prefijo),
# mantenemos esta asignación para evitar errores mientras
# actualizamos la estructura general.
# =============================================================
header <- app_header