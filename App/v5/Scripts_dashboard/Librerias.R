###############################################
# 🔧 Bootstrap de dependencias de la aplicación
# ---------------------------------------------
# Modos:
#   APP_BOOTSTRAP=TRUE  → instala + carga paquetes
#   APP_BOOTSTRAP=FALSE → solo verifica y carga (runtime)
###############################################

# =========================================================
# (0) Configuración de repositorio
# =========================================================
os_name <- Sys.info()[["sysname"]]

if (os_name == "Linux") {
  # Linux / Docker → Posit Package Manager (PPM)
  # pkgType NO se fuerza a "binary" porque Linux no lo soporta.
  # PPM igual entrega paquetes precompilados vía "source".
  options(
    repos = c(PPM = "https://packagemanager.posit.co/cran/__linux__/noble/latest")
  )
} else {
  # Mac / Windows → binarios nativos de CRAN
  options(
    repos   = c(CRAN = "https://cloud.r-project.org"),
    pkgType = "binary"
  )
}

# =========================================================
# (1) Dependencias declaradas de la app
# =========================================================
.core <- c(
  # --- Estructura y UI ---
  "here", "shiny", "shinydashboard", "shinydashboardPlus",
  "shinyWidgets", "shinyjs",

  # --- Manipulación de datos ---
  "readxl", "readr", "openxlsx", "dplyr", "tidyr", "janitor",
  "data.table", "stringi", "scales",

  # --- Visualización ---
  "ggplot2", "highcharter", "reactable", "gt",
  "formattable", "png", "htmltools", "viridisLite",

  # --- Estadística y muestreo ---
  "MASS", "fitdistrplus", "forecast", "jfa",

  # --- Utilidades ---
  "RcppRoll", "sunburstR", "d3r",

  # --- LLM + reportes ---
  "httr2", "jsonlite", "rmarkdown"
)

.heavy <- c(
  "kableExtra", "officer", "flextable",
  "svglite", "ragg", "systemfonts", "textshaping",
  "magick", "rsvg", "pdftools"
)

# =========================================================
# (2) Variables de control
# =========================================================
bootstrap_flag <- Sys.getenv("APP_BOOTSTRAP", "FALSE")
heavy_flag     <- Sys.getenv("APP_HEAVY",     "FALSE")

is_bootstrap <- identical(toupper(bootstrap_flag), "TRUE")
use_heavy    <- identical(toupper(heavy_flag),     "TRUE")

# =========================================================
# (3) Helpers
# =========================================================
.pkgs_missing <- function(pkgs) {
  setdiff(pkgs, rownames(installed.packages()))
}

.instalar_si_faltan <- function(pkgs) {
  faltan <- .pkgs_missing(pkgs)
  if (length(faltan) == 0) {
    message("✅ Todos los paquetes ya están instalados.")
    return(invisible(TRUE))
  }
  message("📦 Instalando (", length(faltan), "): ", paste(faltan, collapse = ", "))
  install.packages(faltan, dependencies = TRUE, quiet = TRUE)
  invisible(TRUE)
}

.verificar_sin_instalar <- function(pkgs, block_name = ".core") {
  faltan <- .pkgs_missing(pkgs)
  if (length(faltan) > 0) {
    stop(
      "Faltan paquetes requeridos en runtime [", block_name, "]: ",
      paste(faltan, collapse = ", "),
      "\n→ Reconstruye la imagen Docker con APP_BOOTSTRAP=TRUE.",
      call. = FALSE
    )
  }
  message("✅ Verificación runtime OK: ", block_name)
  invisible(TRUE)
}

.cargar_todos <- function(pkgs) {
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(
      library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  }))
}

# =========================================================
# (4) Bootstrap vs Runtime
# =========================================================
if (is_bootstrap) {
  message("🚀 BOOTSTRAP: instalando paquetes .core")
  .instalar_si_faltan(.core)
} else {
  message("⚙️  RUNTIME: verificando .core sin instalar")
  .verificar_sin_instalar(.core, ".core")
}
.cargar_todos(.core)

if (use_heavy) {
  if (is_bootstrap) {
    message("💪 BOOTSTRAP + HEAVY: instalando paquetes pesados")
    .instalar_si_faltan(.heavy)
  } else {
    message("💪 RUNTIME + HEAVY: verificando .heavy sin instalar")
    .verificar_sin_instalar(.heavy, ".heavy")
  }
  .cargar_todos(.heavy)
  message("✅ Paquetes pesados cargados.")
} else {
  message("ℹ️  APP_HEAVY=FALSE → paquetes pesados omitidos.")
}

# =========================================================
# (5) Info final
# =========================================================
cat("\n✅ Librerías listas.\n")
if ("here" %in% rownames(installed.packages())) {
  suppressPackageStartupMessages(
    library(here, quietly = TRUE, warn.conflicts = FALSE)
  )
  cat("📂 Raíz del proyecto: ", here(), "\n", sep = "")
}