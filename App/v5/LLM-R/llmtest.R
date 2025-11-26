
install.packages("httr", dependecy=TRUE) 

library(httr)
library(jsonlite)

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
    url   = url,
    body  = body,
    encode = "json",
    timeout(60)
  )
  
  stop_for_status(resp)
  
  content(resp, as = "parsed", type = "application/json")$response
}

# 🔥 TEST
ollama_generate("Explica qué es el muestreo en auditoría en máximo 3 líneas.")
