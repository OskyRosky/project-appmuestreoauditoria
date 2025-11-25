# Imagen base con R (sin shiny-server, nosotros lanzamos la app a mano)
FROM rocker/r-ver:4.4.2

# ─────────────────────────────────────────────
# 1. Paquetes del sistema necesarios para R y librerías "pesadas"
# ─────────────────────────────────────────────
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libmagick++-dev \
    libpoppler-cpp-dev \
    ghostscript \
    && rm -rf /var/lib/apt/lists/*

# ─────────────────────────────────────────────
# 2. Copiar tu app dentro del contenedor
# ─────────────────────────────────────────────
WORKDIR /srv/app
COPY . /srv/app

# ─────────────────────────────────────────────
# 3. Variables de entorno de la app
# ─────────────────────────────────────────────
# APP_BOOTSTRAP: fuerza instalación de librerías en build
# APP_HEAVY: instala también officer, flextable, magick, etc.
# APP_HOST/APP_PORT: usados por AppAuditSample.R
ENV APP_BOOTSTRAP=TRUE \
    APP_HEAVY=TRUE \
    APP_HOST=0.0.0.0 \
    APP_PORT=8000

# ─────────────────────────────────────────────
# 4. Pre-instalar todas las librerías R usando tu propio bootstrap
# ─────────────────────────────────────────────
# Esto ejecuta Librerias.R dentro de Scripts_dashboard y deja todo instalado.
RUN R -q -e "source('Scripts_dashboard/Librerias.R')"

# ─────────────────────────────────────────────
# 5. Exponer el puerto donde corre Shiny
# ─────────────────────────────────────────────
EXPOSE 8000

# ─────────────────────────────────────────────
# 6. Comando por defecto: lanzar tu app
# ─────────────────────────────────────────────
CMD ["R","-q","-f","AppAuditSample.R"]