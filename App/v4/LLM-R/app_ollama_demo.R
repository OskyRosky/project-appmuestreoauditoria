library(shiny)
library(httr2)
library(jsonlite)

# --- Función para hablar con Ollama ---
ollama_generate <- function(prompt) {
  url <- "http://localhost:11434/api/generate"
  
  body <- list(
    model  = "llama3.3:latest",
    prompt = prompt,
    stream = FALSE
  )
  
  resp <- request(url) |>
    req_body_json(body) |>
    req_perform()
  
  out <- resp_body_json(resp)
  out$response
}

ui <- fluidPage(
  tags$head(
    # JS para copiar al portapapeles
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copy_to_clipboard', function(message) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(message.text)
            .then(function() {
              console.log('Texto copiado al portapapeles');
            })
            .catch(function(err) {
              console.error('Error al copiar: ', err);
            });
        } else {
          alert('Tu navegador no soporta copia al portapapeles.');
        }
      });
    "))
  ),
  
  titlePanel("Demo LLM con Ollama + R"),
  
  fluidRow(
    column(
      width = 6,
      h4("Prompt para el LLM:"),
      textAreaInput(
        "prompt",
        label = NULL,
        value = "Cuál es la capital de Nicaragua.",
        rows = 6
      ),
      actionButton("go", "Generar texto con LLM")
    ),
    column(
      width = 6,
      h4("Respuesta del modelo:"),
      verbatimTextOutput("llm_output"),
      br(),
      actionButton("copy", "Copiar respuesta")
    )
  )
)

server <- function(input, output, session) {
  
  # donde guardamos la respuesta actual
  llm_text <- reactiveVal("")
  
  observeEvent(input$go, {
    req(input$prompt)
    txt <- ollama_generate(input$prompt)
    llm_text(txt)
  })
  
  output$llm_output <- renderText({
    llm_text()
  })
  
  # Botón "Copiar respuesta"
  observeEvent(input$copy, {
    txt <- llm_text()
    if (!is.null(txt) && nzchar(txt)) {
      session$sendCustomMessage("copy_to_clipboard", list(text = txt))
    }
  })
}

shinyApp(ui, server)