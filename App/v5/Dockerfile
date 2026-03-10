# =============================================================
# Dockerfile — AppMuestreoAuditoria v5
# App Shiny + Ollama + llama3.2:3b
# Target: linux/amd64  (AWS ECS Fargate)
# =============================================================
#
# CAMBIOS vs versión anterior:
#   1. Posit Package Manager (PPM) → binarios Linux, sin compilar C++
#   2. COPY de la app DESPUÉS de instalar paquetes (aprovecha cache)
#   3. Modelo Ollama pre-descargado en build, no en runtime
#   4. llama3:latest → llama3.2:3b  (2 GB vs 4.7 GB, más rápido en CPU)
#   5. APP_HEAVY=TRUE para instalar officer/flextable (reportes DOCX)
#   6. Eliminado 'stats' de Librerias.R (es paquete base de R)
# =============================================================

FROM rocker/r-ver:4.4.2

# -------------------------------------------------------------
# Variables de entorno
# -------------------------------------------------------------
ENV DEBIAN_FRONTEND=noninteractive
ENV APP_DIR=/app
ENV APP_HOST=0.0.0.0
ENV APP_PORT=8005
ENV OLLAMA_HOST=http://127.0.0.1:11434
ENV OLLAMA_MODEL=llama3.2:3b
ENV APP_BOOTSTRAP=TRUE
ENV APP_HEAVY=TRUE
# PPM: binarios pre-compilados para Ubuntu 24.04 (Noble) → sin compilar C++
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/noble/latest

WORKDIR ${APP_DIR}

# -------------------------------------------------------------
# (1) Dependencias del sistema operativo
# -------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    wget \
    ca-certificates \
    gnupg \
    procps \
    zstd \
    pandoc \
    git \
    # --- librerías para paquetes R ---
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
    # --- magick / rsvg / pdftools (paquetes heavy) ---
    libmagick++-dev \
    librsvg2-dev \
    libpoppler-cpp-dev \
    && rm -rf /var/lib/apt/lists/*

# -------------------------------------------------------------
# (2) Instalar Ollama
# -------------------------------------------------------------
RUN curl -fsSL https://ollama.com/install.sh | sh

# -------------------------------------------------------------
# (3) Instalar paquetes R (ANTES de copiar la app)
#     → Esta capa queda cacheada aunque cambies el código R
#     → PPM entrega binarios: sin compilación C++, ~5 min total
# -------------------------------------------------------------
COPY Scripts_dashboard/Librerias.R /tmp/Librerias.R

RUN R -q -e "\
  options(repos = c(PPM = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); \
  options(pkgType = 'binary'); \
  source('/tmp/Librerias.R') \
"

# -------------------------------------------------------------
# (4) Pre-descargar el modelo Ollama durante el build
#     → El modelo queda en la imagen; no se descarga al arrancar
#     → Requiere que 'ollama serve' esté corriendo durante el build
# -------------------------------------------------------------
RUN ollama serve & \
    sleep 8 && \
    ollama pull ${OLLAMA_MODEL} && \
    pkill ollama || true

# -------------------------------------------------------------
# (5) Copiar el proyecto (DESPUÉS de instalar paquetes)
#     → Cambios en el código no invalidan la cache de R
# -------------------------------------------------------------
COPY . ${APP_DIR}

# -------------------------------------------------------------
# (6) Runtime: sin instalación de paquetes
# -------------------------------------------------------------
ENV APP_BOOTSTRAP=FALSE

# -------------------------------------------------------------
# (7) Script de arranque
# -------------------------------------------------------------
RUN printf '%s\n' \
'#!/usr/bin/env bash' \
'set -e' \
'' \
'echo "============================================"' \
'echo "  AppMuestreoAuditoria v5 — Iniciando..."' \
'echo "============================================"' \
'' \
'# -- Arrancar Ollama en background --' \
'echo "[1/3] Iniciando Ollama..."' \
'ollama serve > /tmp/ollama.log 2>&1 &' \
'' \
'# -- Esperar a que Ollama esté listo (máx 60 s) --' \
'for i in $(seq 1 60); do' \
'  if curl -fsS http://127.0.0.1:11434/api/tags >/dev/null 2>&1; then' \
'    echo "      Ollama listo (${i}s)."' \
'    break' \
'  fi' \
'  sleep 1' \
'done' \
'' \
'# -- Verificar que el modelo esté presente --' \
'echo "[2/3] Verificando modelo ${OLLAMA_MODEL}..."' \
'if ! ollama list | grep -q "${OLLAMA_MODEL%%:*}"; then' \
'  echo "      Modelo no encontrado, descargando (puede tardar)..."' \
'  ollama pull "${OLLAMA_MODEL}"' \
'else' \
'  echo "      Modelo presente. OK."' \
'fi' \
'' \
'# -- Iniciar la app Shiny --' \
'echo "[3/3] Iniciando app Shiny en puerto ${APP_PORT}..."' \
'exec Rscript AppAuditSample.R' \
> /usr/local/bin/start-app.sh \
&& chmod +x /usr/local/bin/start-app.sh

# -------------------------------------------------------------
EXPOSE 8005 11434

CMD ["/usr/local/bin/start-app.sh"]