# Dockerfile para AppAuditSampleLLM (v5)

# 1. Imagen base con R (nosotros lanzamos la app, sin shiny-server)
FROM rocker/r-ver:4.4.2

# 2. Evitar prompts interactivos en las instalaciones
ENV DEBIAN_FRONTEND=noninteractive

# 3. Paquetes de sistema necesarios para R y librerías "pesadas"
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
    curl \
    && rm -rf /var/lib/apt/lists/*

# 4. Copiar TODO el proyecto (AppAuditSample.R, www, data, Scripts_dashboard, LLM-R, etc.)
WORKDIR /srv/AppAuditSampleLLM
COPY . /srv/AppAuditSampleLLM

# 5. Fase de BOOTSTRAP (solo en build):
#    APP_BOOTSTRAP=TRUE → instala paquetes en Librerias.R
ENV APP_BOOTSTRAP=TRUE \
    APP_HEAVY=TRUE

# 6. Pre-instalar todas las librerías R usando tu script de bootstrap
RUN R -q -e "source('Scripts_dashboard/Librerias.R')"

# 7. Variables de entorno para el RUNTIME:
#    APP_BOOTSTRAP=FALSE → YA NO instala, solo carga librerías
ENV APP_BOOTSTRAP=FALSE \
    APP_HEAVY=TRUE \
    APP_HOST=0.0.0.0 \
    APP_PORT=8001

# 8. Exponer el puerto interno donde correrá Shiny
EXPOSE 8001

# 9. Comando por defecto: lanzar tu app de muestreo con LLM
CMD ["R","-q","-f","AppAuditSample.R"]