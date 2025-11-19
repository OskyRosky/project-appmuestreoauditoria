###############################################################
# 🧭 Encabezado (Header) del Dashboard
# -------------------------------------------------------------
# Define la barra superior visible en todas las pestañas.
#
# En esta versión:
#   • Título “Muestreo Auditoría” con icono.
#   • Botón "Welcome Guide" colocado a la izquierda del header.
#   • Dejamos un espacio a la derecha para futuros controles
#     (switch día/noche, chat, compartir, etc.).
###############################################################

app_header <- shinydashboard::dashboardHeader(
  titleWidth = 320,

  # ------------------- Zona izquierda: título + botón guía -------------------
  title = tags$div(
    class = "app-header-left",
    # Icono (puedes cambiarlo por un logo si quieres)
    shiny::icon("chart-bar"),

    # Texto del título
    tags$span(
      "Muestreo Auditoría",
      class = "app-header-title"
    ),

    # Botón Welcome Guide (acción la conectaremos después)
    actionButton(
      inputId = "open_guide",
      label   = "Welcome Guide",
      icon    = icon("info-circle"),
      class   = "btn btn-success btn-sm app-header-guide-btn"
    )
  )

  # ------------------- Zona derecha (vacía por ahora) -------------------
  # En las siguientes etapas agregaremos aquí:
  #   • Switch día/noche
  #   • Icono de feedback (chat)
  #   • Icono de compartir
  #
  # Ejemplo de placeholder (lo dejamos comentado por ahora):
  # ,
  # tags$li(class = "dropdown app-header-right-area")
)

# Alias para compatibilidad con el resto del código
header <- app_header
