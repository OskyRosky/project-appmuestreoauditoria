library(shiny)
library(httr)
library(jsonlite)

# ---- Función que llama a Ollama ----
ollama_generate <- function(prompt,
                            model = "llama3.3:latest",
                            host  = "http://localhost:11434") {
  
  url <- paste0(host, "/api/generate")
  
  body <- list(
    model  = model,
    prompt = prompt,
    stream = FALSE
  )
  
  resp <- POST(
    url    = url,
    body   = body,
    encode = "json",
    timeout(60)
  )
  
  stop_for_status(resp)
  
  content(resp, as = "parsed", type = "application/json")$response
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Demo LLM con Ollama + R"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        inputId = "prompt",
        label   = "Prompt para el LLM:",
        value   = "Explica qué es el muestreo en auditoría en máximo 3 líneas.",
        rows    = 5
      ),
      actionButton(
        inputId = "go",
        label   = "Generar texto con LLM"
      )
    ),
    
    mainPanel(
      h4("Respuesta del modelo:"),
      verbatimTextOutput("llm_text")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  observeEvent(input$go, {
    req(input$prompt)
    
    texto <- input$prompt
    
    # Opcional: barra de progreso
    result <- withProgress(message = "Consultando LLM…", value = 0, {
      incProgress(0.3)
      ans <- ollama_generate(texto)
      incProgress(1)
      ans
    })
    
    output$llm_text <- renderText(result)
  })
}

shinyApp(ui = ui, server = server)