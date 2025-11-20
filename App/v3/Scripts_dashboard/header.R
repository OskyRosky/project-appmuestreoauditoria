###############################################################
# 🧭 Encabezado (Header) del Dashboard
###############################################################

app_header <- shinydashboard::dashboardHeader(
  titleWidth = 260,
  
  # --- Título (icono + texto) ---
  title = tagList(
    tags$span(
      icon("chart-bar"),
      tags$span(
        "Muestreo Auditoría",
        style = "font-weight:600; margin-left:6px;"
      )
    )
  ),
  
  # --- Welcome Guide (posicionado cerca de la izquierda) ---
  tags$li(
    class = "dropdown",
    style = "position:absolute; left:270px; top:4px; padding:0 10px;",
    actionButton(
      inputId = "welcome_guide",
      label = tagList(
        icon("info-circle"),
        tags$span("Welcome Guide", style = "margin-left:4px;")
      ),
      class = "btn btn-success btn-sm"
    )
  ),
  
  # --- ÍCONO DE CHAT (feedback por correo) -------------------
  shinydashboard::dropdownMenu(
    type = "messages",
    icon = icon("comment"),
    badgeStatus = NULL,
    shinydashboard::messageItem(
      from = "Comentarios y sugerencias",
      message = "Envíe sus comentarios por correo",
      icon   = icon("envelope"),
      href   = "mailto:muestreo_auditoria@cgr.go.cr"
    )
  ),
  
  # --- ÍCONO DE COMPARTIR (redes sociales) -------------------
  shinydashboard::dropdownMenu(
    type = "notifications",
    icon = icon("share-alt"),
    badgeStatus = NULL,
    shinydashboard::notificationItem(
      text = "Twitter",
      icon = icon("twitter"),
      href = "https://twitter.com/intent/tweet?text=Muestreo%20de%20Auditor%C3%ADa"
    ),
    shinydashboard::notificationItem(
      text = "Facebook",
      icon = icon("facebook"),
      href = "https://www.facebook.com/sharer/sharer.php?u=#"
    ),
    shinydashboard::notificationItem(
      text = "Google+",
      icon = icon("google-plus"),
      href = "https://plus.google.com/share?url=#"
    )
  ),
  
  # --- Switch Day / Night (EXTREMO DERECHO) ------------------
  tags$li(
    class = "dropdown",
    style = "padding: 12px 20px 0 20px; margin-left:auto;",
    shinyWidgets::materialSwitch(
      inputId = "dark_mode",
      label  = NULL,
      value  = FALSE,
      status = "primary",
      right  = TRUE
    )
  )
)

# Alias para compatibilidad
header <- app_header