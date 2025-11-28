###############################################
# 🔧 Bootstrap de dependencias de la aplicación
# ---------------------------------------------
# Este script asegura que todas las librerías
# necesarias estén instaladas y cargadas antes
# de ejecutar la App de Muestreo de Auditoría.
###############################################

# =========================================================
# (0) Configuración del mirror CRAN
# =========================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))

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
# =========================================================

.core <- c(
  # --- Sistema base y estructura de app ---
  "here", "shiny", "shinydashboard", "shinydashboardPlus",
  "shinyWidgets", "shinyjs",

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
  "httr2", "jsonlite", "rmarkdown"
)

.heavy <- c(
  # --- Reportes en Word y tablas ricas ---
  "kableExtra", "officer", "flextable",

  # --- Gráficos y renderizado avanzado ---
  "svglite", "ragg", "systemfonts", "textshaping",
  "magick", "rsvg", "pdftools"
)

# =========================================================
# (2) Variables de control (desde entorno)
# =========================================================
bootstrap_flag <- Sys.getenv("APP_BOOTSTRAP", "FALSE")
heavy_flag     <- Sys.getenv("APP_HEAVY",     "FALSE")

# =========================================================
# (3) Helpers de instalación / carga
# =========================================================
.instalar_si_faltan <- function(pkgs, force = FALSE) {
  ya_instalados <- rownames(installed.packages())
  faltan <- if (force) pkgs else setdiff(pkgs, ya_instalados)
  if (length(faltan)) {
    message("📦 Instalando paquetes: ", paste(faltan, collapse = ", "))
    install.packages(faltan, dependencies = TRUE, quiet = TRUE)
  }
}

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
# (4) Bootstrap según APP_BOOTSTRAP
# =========================================================
if (identical(bootstrap_flag, "TRUE")) {
  message("🚀 Modo BOOTSTRAP (APP_BOOTSTRAP=TRUE): instalando y cargando paquetes .core")
  .instalar_si_faltan(.core, force = TRUE)
} else {
  message("⚙️  Modo RUNTIME (APP_BOOTSTRAP=FALSE): solo se verifica y cargan paquetes .core")
  .instalar_si_faltan(.core, force = FALSE)
}

.cargar_todos(.core)

# --- (4.1) Paquetes pesados solo si APP_HEAVY=TRUE ---
if (identical(heavy_flag, "TRUE")) {
  message("💪 APP_HEAVY=TRUE → incluyendo paquetes pesados")
  if (identical(bootstrap_flag, "TRUE")) {
    .instalar_si_faltan(.heavy, force = TRUE)
  } else {
    .instalar_si_faltan(.heavy, force = FALSE)
  }
  .cargar_todos(.heavy)
  message("✅ Paquetes pesados cargados correctamente.")
} else {
  message("ℹ️ APP_HEAVY=FALSE → paquetes pesados omitidos.")
}

# =========================================================
# (5) Información del entorno
# =========================================================
suppressMessages(library(here))
cat("\n✅ Librerías listas y entorno inicializado correctamente.\n")
cat("📂 Raíz del proyecto detectada por {here}: ", here(), "\n", sep = "")