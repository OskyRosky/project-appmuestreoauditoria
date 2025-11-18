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
  titleWidth = 280,  # 🔹 Más ancho para que quepa el texto completo
  title = tagList(
    # Icono tipo logo (fontawesome)
    shiny::icon("chart-bar"),
    # Texto del título
    tags$span(
      "📊 Muestreo de Auditoría",
      style = "font-weight:600; margin-left:8px;"
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