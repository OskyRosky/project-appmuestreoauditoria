###############################################################
# 🧭 Barra lateral (Sidebar) del Dashboard
# -------------------------------------------------------------
# Este script define el menú de navegación principal que aparece
# en el lado izquierdo del dashboard. Cada elemento del menú
# se asocia con un `tabName` en el `dashboardBody`.
#
# Contiene:
#   • Menú principal con 6 secciones funcionales.
#   • Íconos visuales de apoyo (Font Awesome).
#   • Preparado para incluir submenús o tooltips en el futuro.
#
# NOTA:
#   Los tabName definidos aquí deben coincidir exactamente
#   con los usados en el archivo `body.R`.
###############################################################

# =============================================================
# (1) Función auxiliar: íconos seguros
# -------------------------------------------------------------
safe_icon <- function(name, fallback = "table") {
  tryCatch(icon(name), error = function(...) icon(fallback))
}

# =============================================================
# (2) Definición del sidebar
# -------------------------------------------------------------
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "sidebar",     # 👈 ***NUEVO: necesario para botón dinámico***

    shinydashboard::menuItem(
      text    = "Presentación",
      tabName = "p1",
      icon    = safe_icon("chalkboard")
    ),
    shinydashboard::menuItem(
      text    = "Descriptivo",
      tabName = "p2",
      icon    = safe_icon("chart-bar")
    ),
    shinydashboard::menuItem(
      text    = "Muestra MUM",
      tabName = "p3",
      icon    = safe_icon("list-ol")
    ),
    shinydashboard::menuItem(
      text    = "Muestra LES",
      tabName = "p4",
      icon    = safe_icon("filter")
    ),
    shinydashboard::menuItem(
      text    = "Muestra Atributos",
      tabName = "p5",
      icon    = safe_icon("tags")
    ),
    shinydashboard::menuItem(
      text    = "Evaluación",
      tabName = "p6",
      icon    = safe_icon("ruler")
    ),

    # ---- NUEVA SECCIÓN: FAQs --------------------------------
    shinydashboard::menuItem(
      text    = "FAQs",
      tabName = "faq",
      icon    = safe_icon("question-circle")
    )
  )
)

# (3) Alias opcional (compatibilidad) -------------------------
sider <- sidebar