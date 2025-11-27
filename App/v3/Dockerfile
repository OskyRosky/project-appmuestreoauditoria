# 1. Paquetes del sistema necesarios para R y librerías "pesadas"
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

# 2. Copiar todo el proyecto al contenedor
WORKDIR /srv/app
COPY . /srv/app

# 3. Variables de entorno para tu app
#    APP_BOOTSTRAP / APP_HEAVY: usados por Scripts_dashboard/Librerias.R
#    APP_HOST / APP_PORT: usados por AppAuditSample.R para runApp()
ENV APP_BOOTSTRAP=TRUE \
    APP_HEAVY=TRUE \
    APP_HOST=0.0.0.0 \
    APP_PORT=8001

# 4. Pre-instalar librerías R usando tu bootstrap
RUN R -q -e "source('Scripts_dashboard/Librerias.R')"

# 5. Exponer el puerto donde corre Shiny dentro del contenedor
EXPOSE 8001

# 6. Comando por defecto: lanzar tu app de muestreo con LLM
CMD ["R","-q","-f","AppAuditSample.R"]