# Imagen base con R + Shiny Server
FROM rocker/shiny:4.4.1

# Evitar prompts interactivos
ENV DEBIAN_FRONTEND=noninteractive

# Dependencias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar los paquetes que usa la app
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
  'jsonlite' \
), repos = 'https://cloud.r-project.org')"

# Directorio donde Shiny Server sirve apps
WORKDIR /srv/shiny-server/

# Copiar pasta completa (la app Shiny)
COPY . /srv/shiny-server/

# Exponer puerto 3838 (Shiny Server)
EXPOSE 3838

# Comando que corre shiny-server
CMD ["/usr/bin/shiny-server"]