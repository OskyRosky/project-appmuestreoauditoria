# Dockerfile para AppMuestreoAuditoria v5
#
# Objetivo:
# - Empaquetar la app Shiny + Ollama + modelo llama3:latest
# - Preparado para pruebas locales
# - Compatible con linux/amd64 (x86_64), pensando en ECS + Fargate
# - Puerto de la app: 8005
#
# Nota:
# - Este contenedor es monolítico:
#     * corre Ollama dentro del mismo contenedor
#     * corre la app R/Shiny dentro del mismo contenedor

# 1. Imagen base con R, forzada a linux/amd64
FROM rocker/r-ver:4.4.2

# 2. Variables globales del contenedor
ENV DEBIAN_FRONTEND=noninteractive
ENV APP_DIR=/app
ENV APP_HOST=0.0.0.0
ENV APP_PORT=8005
ENV OLLAMA_HOST=http://127.0.0.1:11434
ENV OLLAMA_MODEL=llama3:latest
ENV PATH=/usr/local/bin:$PATH

# 3. Directorio principal de trabajo
WORKDIR /app

# 4. Instalar dependencias del sistema
#
# Incluye:
# - utilitarios base
# - librerías de compilación para paquetes R
# - pandoc para reportes/docx/rmarkdown
# - procps para inspección de procesos
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    ca-certificates \
    gnupg \
    software-properties-common \
    zstd \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libicu-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libglpk40 \
    libglpk-dev \
    libxt6 \
    libx11-6 \
    libxt-dev \
    pandoc \
    pandoc-data \
    git \
    procps \
    && rm -rf /var/lib/apt/lists/*

# 5. Instalar Ollama dentro del contenedor
RUN curl -fsSL https://ollama.com/install.sh | sh

# 6. Copiar todo el proyecto al contenedor
COPY . ${APP_DIR}

# 7. Instalar gestor de paquetes R moderno
RUN R -e "install.packages('pak', repos='https://cloud.r-project.org')"

# 8. Instalar paquetes R necesarios para la app
#
# Aquí dejamos explícitos los principales paquetes usados por:
# - Shiny
# - tablas
# - gráficos
# - lectura/escritura de archivos
# - reportes DOCX
# - consumo de Ollama vía HTTP
RUN R -e "pak::pkg_install(c( \
  'shiny','shinydashboard','shinyWidgets','shinyjs','reactable','DT', \
  'dplyr','tidyr','tibble','purrr','stringr','readr','readxl','writexl','openxlsx', \
  'ggplot2','plotly','highcharter','htmlwidgets','scales', \
  'officer','flextable','rmarkdown','knitr', \
  'httr2','jsonlite','xml2','lubridate' \
))"

# 9. Crear carpeta donde Ollama guarda modelos
RUN mkdir -p /root/.ollama

# 10. Crear script de arranque del contenedor
#
# Flujo:
# - inicia ollama serve
# - espera a que Ollama responda
# - verifica si llama3:latest existe
# - si no existe, lo descarga
# - luego arranca la app Shiny
RUN printf '%s\n' \
'#!/usr/bin/env bash' \
'set -e' \
'' \
'echo "Iniciando Ollama..."' \
'ollama serve > /tmp/ollama.log 2>&1 &' \
'' \
'for i in $(seq 1 60); do' \
'  if curl -fsS http://127.0.0.1:11434/api/tags >/dev/null 2>&1; then' \
'    echo "Ollama listo."' \
'    break' \
'  fi' \
'  sleep 1' \
'done' \
'' \
'echo "Verificando modelo ${OLLAMA_MODEL}..."' \
'if ! ollama list | grep -q "${OLLAMA_MODEL%%:*}"; then' \
'  echo "Descargando modelo ${OLLAMA_MODEL}..."' \
'  ollama pull "${OLLAMA_MODEL}"' \
'else' \
'  echo "Modelo ya presente."' \
'fi' \
'' \
'echo "Iniciando app Shiny en ${APP_HOST}:${APP_PORT}..."' \
'exec Rscript AppAuditSample.R' \
> /usr/local/bin/start-app.sh \
&& chmod +x /usr/local/bin/start-app.sh

# 11. Exponer puertos
#
# 8005  -> app Shiny
# 11434 -> Ollama interno
EXPOSE 8005 11434

# 12. Comando por defecto del contenedor
CMD ["/usr/local/bin/start-app.sh"]