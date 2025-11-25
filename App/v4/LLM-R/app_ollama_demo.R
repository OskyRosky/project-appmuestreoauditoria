
library(shiny)
library(httr2)
library(jsonlite)
library(tinytex)
library(rmarkdown)

# --- Función para hablar con Ollama ---
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
  out$response
}

# ui ----
ui <- fluidPage(
  titlePanel("Demo LLM con Ollama + R"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("prompt", "Prompt para el LLM:", rows = 6),
      actionButton("go", "Generar texto con LLM"),
      br(), br(),
      downloadButton("download_pdf", "Descargar respuesta en PDF")
    ),
    mainPanel(
      h4("Respuesta del modelo:"),
      verbatimTextOutput("llm_output")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  llm_text <- reactiveVal("")
  
  observeEvent(input$go, {
    req(input$prompt)
    
    withProgress(message = "Consultando LLM...", value = 0, {
      ans <- ollama_generate(input$prompt)
      llm_text(ans)
      output$llm_output <- renderText(ans)
      incProgress(1)
    })
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("informe_llm_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
    },
    content = function(file) {
      req(llm_text())
      
      # 1) Rmd temporal
      tmp_rmd <- tempfile(fileext = ".Rmd")
      
      rmd_lines <- c(
        "---",
        "title: \"Informe generado con LLM\"",
        "output: pdf_document",
        "---",
        "",
        "## Respuesta del modelo",
        "",
        llm_text()
      )
      
      writeLines(rmd_lines, tmp_rmd)
      
      # 2) Render a PDF usando tinytex
      rmarkdown::render(
        input        = tmp_rmd,
        output_file  = file,
        output_format = "pdf_document",
        quiet        = TRUE,
        envir        = new.env(parent = globalenv())
      )
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)