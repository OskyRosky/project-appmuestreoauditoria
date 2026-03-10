# Dockerfile para AppMuestreoAuditoria v5
# App Shiny + Ollama + llama3
# Preparado para linux/amd64 (ECS/Fargate)

FROM rocker/r-ver:4.4.2

# -------------------------------------------------------
# Variables del contenedor
# -------------------------------------------------------

ENV DEBIAN_FRONTEND=noninteractive
ENV APP_DIR=/app
ENV APP_HOST=0.0.0.0
ENV APP_PORT=8005
ENV OLLAMA_HOST=http://127.0.0.1:11434
ENV OLLAMA_MODEL=llama3:latest
ENV APP_BOOTSTRAP=TRUE

WORKDIR /app

# -------------------------------------------------------
# Dependencias del sistema
# -------------------------------------------------------

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

# -------------------------------------------------------
# Instalar Ollama
# -------------------------------------------------------

RUN curl -fsSL https://ollama.com/install.sh | sh

# -------------------------------------------------------
# Copiar proyecto
# -------------------------------------------------------

COPY . ${APP_DIR}

# -------------------------------------------------------
# Instalar pak
# -------------------------------------------------------

RUN R -e "install.packages('pak', repos='https://cloud.r-project.org')"

# -------------------------------------------------------
# Instalar librerías R usando tu script
# (modo bootstrap)
# -------------------------------------------------------

RUN R -q -e "source('Scripts_dashboard/Librerias.R')"

# -------------------------------------------------------
# Runtime ya NO instala paquetes
# -------------------------------------------------------

ENV APP_BOOTSTRAP=FALSE

# -------------------------------------------------------
# Carpeta de modelos Ollama
# -------------------------------------------------------

RUN mkdir -p /root/.ollama

# -------------------------------------------------------
# Script de arranque
# -------------------------------------------------------

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
'echo "Iniciando app Shiny..."' \
'exec Rscript AppAuditSample.R' \
> /usr/local/bin/start-app.sh \
&& chmod +x /usr/local/bin/start-app.sh

# -------------------------------------------------------

EXPOSE 8005 11434

CMD ["/usr/local/bin/start-app.sh"]