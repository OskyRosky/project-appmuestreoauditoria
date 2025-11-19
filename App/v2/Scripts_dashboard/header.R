###############################################################
# 🧭 Encabezado (Header) del Dashboard
# -------------------------------------------------------------
# Este script define la barra superior de la aplicación Shiny,
# visible en todas las pestañas del dashboard.
#
# Define la barra superior visible en todas las pestañas.
#
# En esta versión:
#   • Título “Muestreo Auditoría” con icono.
#   • Botón "Welcome Guide" colocado a la izquierda del header.
#   • Dejamos un espacio a la derecha para futuros controles
#     (switch día/noche, chat, compartir, etc.).
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
  title = "📊 Muestreo de Auditoría"  # título mostrado en la barra superior
)

# =============================================================
# (2) Alias para compatibilidad retroactiva
# -------------------------------------------------------------
# Si el código anterior hacía referencia a `header` (sin prefijo),
# mantenemos esta asignación para evitar errores mientras
# actualizamos la estructura general.
# =============================================================
header <- app_header