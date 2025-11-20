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
  
  # --- Welcome Guide (después del toggle sidebar) ---
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
  
  # --- Dropdown Menu: Comentarios/Feedback (centro-derecha) ---
  tags$li(
    class = "dropdown",
    tags$a(
      href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      icon("comment"),
      tags$span(class = "label label-warning", "")
    ),
    tags$ul(
      class = "dropdown-menu",
      tags$li(
        tags$ul(
          class = "menu",
          tags$li(
            tags$a(
              href = "mailto:tu_email@ejemplo.com",
              icon("envelope"),
              " Enviar comentarios"
            )
          )
        )
      )
    )
  ),
  
  # --- Dropdown Menu: Compartir en redes sociales (centro-derecha) ---
  tags$li(
    class = "dropdown",
    tags$a(
      href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      icon("share-alt"),
      tags$span(class = "label label-success", "")
    ),
    tags$ul(
      class = "dropdown-menu",
      tags$li(
        tags$ul(
          class = "menu",
          tags$li(
            tags$a(
              href = "https://twitter.com/intent/tweet?text=Muestreo%20Auditoría",
              target = "_blank",
              icon("twitter"),
              " Twitter"
            )
          ),
          tags$li(
            tags$a(
              href = "https://www.facebook.com/sharer/sharer.php?u=tu_url_aqui",
              target = "_blank",
              icon("facebook"),
              " Facebook"
            )
          ),
          tags$li(
            tags$a(
              href = "https://www.linkedin.com/sharing/share-offsite/?url=tu_url_aqui",
              target = "_blank",
              icon("linkedin"),
              " LinkedIn"
            )
          )
        )
      )
    )
  ),
  
  # --- Switch Day / Night (EXTREMO DERECHO) ---
  tags$li(
    class = "dropdown",
    style = "padding: 12px 20px 0 20px; margin-left: auto;",
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