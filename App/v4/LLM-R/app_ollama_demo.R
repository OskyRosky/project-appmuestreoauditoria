library(shiny)
library(httr2)
library(jsonlite)
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

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("Demo LLM con Ollama + R"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("prompt", "Prompt para el LLM:", rows = 6),
      actionButton("go", "Generar texto con LLM"),
      br(), br(),
      
      # 🔽 Solo se muestra cuando hay respuesta del modelo
      conditionalPanel(
        condition = "output.show_download",
        downloadButton("download_docx", "Descargar respuesta en .docx")
      )
    ),
    mainPanel(
      h4("Respuesta del modelo:"),
      verbatimTextOutput("llm_output")
    )
  )
)

# --------------- SERVER ---------------
server <- function(input, output, session) {
  
  llm_text <- reactiveVal("")
  
  # Para mostrar / ocultar el botón de descarga
  output$show_download <- reactive({
    txt <- llm_text()
    !is.null(txt) && nzchar(txt)
  })
  outputOptions(output, "show_download", suspendWhenHidden = FALSE)
  
  observeEvent(input$go, {
    req(input$prompt)
    
    withProgress(message = "Consultando LLM...", value = 0, {
      ans <- ollama_generate(input$prompt)
      llm_text(ans)
      output$llm_output <- renderText(ans)
      incProgress(1)
    })
  })
  
  # ----- Descargar en DOCX -----
  output$download_docx <- downloadHandler(
    filename = function() {
      paste0("informe_llm_", format(Sys.time(), "%Y%m%d_%H%M"), ".docx")
    },
    content = function(file) {
      req(llm_text())
      
      # 1) Rmd temporal
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
      
      # 2) Render a Word (.docx) con rmarkdown/pandoc
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