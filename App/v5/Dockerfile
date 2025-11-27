# Imagen base con R + Shiny Server (incluye R y shiny-server ya configurado)
FROM rocker/shiny:4.4.1

# Para evitar prompts interactivos en la instalación
ENV DEBIAN_FRONTEND=noninteractive

# Paquetes del sistema que algunas libs de R necesitan
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar los paquetes de R necesarios para la app
RUN R -e "install.packages(c( \
  'shiny', \
  'shinydashboard', \
  'shinyjs', \
  'reactable', \
  'highcharter', \
  'openxlsx', \
  'officer', \
  'flextable', \
  'ggplot2', \
  'dplyr', \
  'readxl', \
  'httr', \
  'jsonlite', \
  'tidyr' \
), repos = 'https://cloud.r-project.org')"

# Si usas un paquete específico para `planning()` (por ejemplo 'audit'),
# descomenta esta línea:
# RUN R -e "install.packages('audit', repos = 'https://cloud.r-project.org')"

# Copiar TODO el contenido de v5 dentro de una subcarpeta 'app'
# Shiny Server servirá esta app en /app
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/app

# Exponer el puerto que usa shiny-server
EXPOSE 3838

# Comando por defecto: lanzar shiny-server
CMD ["/usr/bin/shiny-server"]