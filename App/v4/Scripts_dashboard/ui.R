###############################################################
# 📄 ui.R — Estructura principal del UI (shinydashboard)
# -------------------------------------------------------------
# Ensambla los objetos:
#   - header  : definido en header.R  -> dashboardHeader(...)
#   - sidebar : definido en sider.R   -> dashboardSidebar(...)
#   - body    : definido en body.R    -> dashboardBody(...)
#
# Buenas prácticas:
#   • Validación temprana de objetos requeridos.
#   • NO imprimir objetos sueltos (p.ej. una línea con `header` sola).
#   • Pasar cada objeto tal cual a dashboardPage (sin envolver en tagList).
###############################################################

############################
#   Contenido del UI       #
#   (definición de ui)     #
############################

# (1) Validaciones mínimas (fallan temprano si algo no está)
stopifnot("El objeto `header` no existe."  = exists("header",  inherits = FALSE))
stopifnot("El objeto `sidebar` no existe." = exists("sidebar", inherits = FALSE))
stopifnot("El objeto `body` no existe."    = exists("body",    inherits = FALSE))

# (2) Verificación de tipo (deben ser shiny.tag o shiny.tag.list)
ok_header  <- inherits(header,  "shiny.tag") || inherits(header,  "shiny.tag.list")
ok_sidebar <- inherits(sidebar, "shiny.tag") || inherits(sidebar, "shiny.tag.list")
ok_body    <- inherits(body,    "shiny.tag") || inherits(body,    "shiny.tag.list")

if (!ok_header)  stop("`header` no es un shiny.tag válido. Revisa header.R")
if (!ok_sidebar) stop("`sidebar` no es un shiny.tag válido. Revisa sider.R")
if (!ok_body)    stop("`body` no es un shiny.tag válido. Revisa body.R")

# (3) Construcción del dashboard — NO envolver header en tagList, ni imprimirlo
ui <- shinydashboard::dashboardPage(
  title   = "Muestreo | CGR",  # Título en pestaña del navegador
  skin    = "blue",
  header  = header,   # <- PASA EL OBJETO TAL CUAL
  sidebar = sidebar,  # <- PASA EL OBJETO TAL CUAL
  body    = body      # <- PASA EL OBJETO TAL CUAL
)

# (4) Nada más al final (no imprimir `ui`).