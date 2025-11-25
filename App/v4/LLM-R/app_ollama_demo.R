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

ui <- fluidPage(
  titlePanel("Demo LLM con Ollama + R"),
  
  fluidRow(
    column(
      width = 6,
      h4("Prompt para el LLM:"),
      textAreaInput(
        "prompt",
        label = NULL,
        value = "Explica qué es el muestreo en auditoría en máximo 3 líneas.",
        rows = 8
      ),
      actionButton("go", "Generar texto con LLM")
    ),
    column(
      width = 6,
      h4("Respuesta del modelo:"),
      verbatimTextOutput("llm_output"),
      br(),
      downloadButton("download_pdf", "Descargar respuesta en PDF")
    )
  )
)

server <- function(input, output, session) {
  
  # Donde guardamos la última respuesta del LLM
  llm_text <- reactiveVal("")
  
  # Botón para generar texto con barra de progreso
  observeEvent(input$go, {
    req(input$prompt)
    
    withProgress(message = "Consultando modelo LLM...", value = 0, {
      # 1) Llamamos al modelo
      txt <- tryCatch(
        {
          incProgress(0.4, detail = "Generando respuesta...")
          ollama_generate(input$prompt)
        },
        error = function(e) {
          showNotification("Error al consultar el modelo LLM.", type = "error")
          return("Ocurrió un error al consultar el modelo LLM.")
        }
      )
      
      # 2) Guardamos texto y actualizamos salida
      incProgress(0.9, detail = "Procesando resultado...")
      llm_text(txt)
      
      incProgress(1)
    })
  })
  
  # Mostrar respuesta en pantalla
  output$llm_output <- renderText({
    llm_text()
  })
  
  # Descargar en PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("respuesta_llm_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(llm_text())
      req(input$prompt)
      
      # Creamos un Rmd temporal con prompt + respuesta
      tmp_rmd <- tempfile(fileext = ".Rmd")
      
      rmd_content <- paste0(
        "---\n",
        "title: \"Informe LLM\"\n",
        "output: pdf_document\n",
        "geometry: margin=2cm\n",
        "---\n\n",
        "## Prompt utilizado\n\n",
        "```text\n",
        input$prompt, "\n",
        "```\n\n",
        "## Respuesta del modelo\n\n",
        llm_text(), "\n"
      )
      
      writeLines(rmd_content, tmp_rmd)
      
      # Renderizamos a PDF
      rmarkdown::render(
        input        = tmp_rmd,
        output_file  = file,
        envir        = new.env(parent = globalenv()),
        quiet        = TRUE
      )
    }
  )
}

shinyApp(ui, server)