###############################################
# 🔧 Bootstrap de dependencias de la aplicación
# ---------------------------------------------
# Este script asegura que todas las librerías
# necesarias estén instaladas y cargadas antes
# de ejecutar la App de Muestreo de Auditoría.
#
# Funciones clave:
#   • Verifica qué paquetes están instalados.
#   • Instala automáticamente los que falten.
#   • Permite forzar reinstalación con una variable
#     de entorno (APP_BOOTSTRAP=TRUE).
#   • Carga silenciosamente todos los paquetes.
#   • Muestra la raíz del proyecto detectada por {here}.
###############################################

# =========================================================
# (0) Configuración del mirror CRAN
# ---------------------------------------------------------
# Evita el prompt interactivo al instalar paquetes y
# garantiza consistencia entre entornos Windows/Mac/Linux.
# =========================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Elegir tipo de paquete según SO:
os_name <- Sys.info()[["sysname"]]

if (os_name %in% c("Darwin", "Windows")) {
  # Mac / Windows → usar binarios (más rápido)
  options(pkgType = "binary")
} else {
  # Linux (por ejemplo, dentro de Docker) → usar source
  options(pkgType = "source")
}

# =========================================================
# (1) Listado de dependencias de la App
# ---------------------------------------------------------
# Incluye librerías de UI (Shiny), análisis estadístico,
# visualización, manejo de datos y generación de reportes.
# =========================================================
.core <- c(
  # --- Sistema base y estructura de app ---
  "here", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",

  # --- Manipulación y limpieza de datos ---
  "readxl", "readr", "openxlsx", "dplyr", "tidyr", "janitor",
  "data.table", "stringi", "scales",

  # --- Visualización y tableros ---
  "ggplot2", "highcharter", "reactable", "gt",
  "formattable", "png", "htmltools", "viridisLite",

  # --- Estadística, modelado y muestreo ---
  "stats", "MASS", "fitdistrplus", "forecast", "jfa",

  # --- Utilidades y soporte ---
  "RcppRoll", "sunburstR", "d3r",

  # --- 🔁 Integración LLM (Ollama) + reportes DOCX ---
  # httr2/jsonlite → llamadas HTTP y parseo JSON
  # rmarkdown      → generación de Rmd / informes si se requiere
  "httr2", "jsonlite", "rmarkdown"
)

# Paquetes “pesados” que requieren librerías nativas del sistema (macOS/Linux)
.heavy <- c(
  # --- Reportes en Word y tablas ricas ---
  "kableExtra", "officer", "flextable",

  # --- Gráficos y renderizado avanzado ---
  "svglite", "ragg", "systemfonts", "textshaping",
  "magick", "rsvg", "pdftools"
)

# =========================================================
# (2) Variables de control
# ---------------------------------------------------------
# APP_BOOTSTRAP = TRUE → fuerza reinstalación.
# APP_HEAVY = TRUE → incluye paquetes pesados.
# =========================================================
.force_install  <- isTRUE(as.logical(Sys.getenv("APP_BOOTSTRAP", "FALSE")))
.install_heavy  <- isTRUE(as.logical(Sys.getenv("APP_HEAVY", "FALSE")))

# =========================================================
# (3) Función para instalar paquetes faltantes
# ---------------------------------------------------------
.instalar_si_faltan <- function(pkgs, force = FALSE) {
  ya_instalados <- rownames(installed.packages())
  faltan <- if (force) pkgs else setdiff(pkgs, ya_instalados)
  if (length(faltan)) {
    message("📦 Instalando paquetes: ", paste(faltan, collapse = ", "))
    install.packages(faltan, dependencies = TRUE, quiet = TRUE)
  }
}

# =========================================================
# (4) Función para cargar librerías silenciosamente
# ---------------------------------------------------------
.cargar_todos <- function(pkgs) {
  invisible(lapply(
    pkgs,
    function(p)
      suppressMessages(
        library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
  ))
}

# =========================================================
# (5) Ejecutar el bootstrap de dependencias
# ---------------------------------------------------------
# Instala (si falta) y luego carga todas las librerías base.
# =========================================================
.instalar_si_faltan(.core, force = .force_install)
.cargar_todos(.core)

# --- (5.1) Cargar paquetes pesados solo si APP_HEAVY=TRUE ---
if (.install_heavy) {
  try({
    .instalar_si_faltan(.heavy, force = .force_install)
    .cargar_todos(.heavy)
    message("✅ Paquetes pesados cargados correctamente.")
  }, silent = TRUE)
} else {
  message("ℹ️ Paquetes pesados omitidos (usa APP_HEAVY=TRUE para incluirlos).")
}

# =========================================================
# (6) Información del entorno
# ---------------------------------------------------------
# Carga {here} y muestra la raíz del proyecto detectada.
# Esto facilita trazabilidad y validación de rutas.
# =========================================================
suppressMessages(library(here))
cat("\n✅ Librerías listas y entorno inicializado correctamente.\n")
cat("📂 Raíz del proyecto detectada por {here}: ", here(), "\n", sep = "")