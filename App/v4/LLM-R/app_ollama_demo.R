
library(shiny)
library(httr2)
library(jsonlite)
library(rmarkdown)

# ---------- 1. Ver si Ollama está disponible ----------
ollama_available <- function() {
  url <- "http://localhost:11434/api/tags"
  tryCatch({
    request(url) |> req_perform()
    TRUE
  }, error = function(e) FALSE)
}

# ---------- 2. Función para llamar al LLM de Ollama ----------
ollama_generate <- function(prompt) {
  url <- "http://localhost:11434/api/generate"
  
  body <- list(
    model  = "llama3.3:latest",  # ajusta si usas otro modelo
    prompt = prompt,
    stream = FALSE
  )
  
  resp <- request(url) |>
    req_body_json(body) |>
    req_perform()
  
  out <- resp_body_json(resp)
  enc2utf8(out$response)   # asegurar UTF-8
}

# ---------- 3. UI ----------
ui <- fluidPage(
  titlePanel("Demo LLM con Ollama + R"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("prompt", "Prompt para el LLM:", rows = 6),
      actionButton("go", "Generar texto con LLM"),
      br(), br(),
      downloadButton("download_docx", "Descargar respuesta en .docx")
    ),
    mainPanel(
      h4("Respuesta del modelo:"),
      tags$div(
        style = "background:#f8f9fa; border:1px solid #ccc; padding:12px; border-radius:6px;",
        verbatimTextOutput("llm_output")
      )
    )
  )
)

# ---------- 4. Server ----------
server <- function(input, output, session) {
  
  llm_text <- reactiveVal("")
  
  observeEvent(input$go, {
    req(input$prompt)
    
    if (!ollama_available()) {
      showNotification("❌ Ollama no está activo. Ejecuta 'ollama serve' en la terminal.", type = "error")
      return()
    }
    
    withProgress(message = "Generando respuesta...", value = 0, {
      incProgress(0.2)
      ans <- ollama_generate(input$prompt)
      incProgress(0.9)
      llm_text(ans)
      output$llm_output <- renderText(ans)
      incProgress(1)
    })
  })
  
  # ---------- 5. Descarga en DOCX ----------
  output$download_docx <- downloadHandler(
    filename = function() {
      paste0("informe_llm_", format(Sys.time(), "%Y%m%d_%H%M"), ".docx")
    },
    content = function(file) {
      req(llm_text())
      
      # Rmd temporal
      tmp_rmd <- tempfile(fileext = ".Rmd")
      
      rmd_lines <- c(
        "---",
        "title: \"Informe generado con LLM\"",
        "output: word_document",
        "---",
        "",
        "## Respuesta del modelo",
        "",
        llm_text()
      )
      
      writeLines(rmd_lines, tmp_rmd)
      
      rmarkdown::render(
        input         = tmp_rmd,
        output_file   = file,
        output_format = "word_document",
        quiet         = TRUE,
        envir         = new.env(parent = globalenv())
      )
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
}

shinyApp(ui, server)