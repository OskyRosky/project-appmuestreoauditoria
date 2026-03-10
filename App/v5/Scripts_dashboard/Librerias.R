###############################################
# 🔧 Bootstrap de dependencias de la aplicación
# ---------------------------------------------
# Este script controla dos modos:
#
# 1) BOOTSTRAP  -> instala + carga paquetes
#    APP_BOOTSTRAP=TRUE
#
# 2) RUNTIME    -> NO instala nada
#    APP_BOOTSTRAP=FALSE
#    Solo verifica que todo exista y luego carga.
#
# Esto evita que el contenedor Docker se quede
# instalando paquetes cada vez que arranca.
###############################################

# =========================================================
# (0) Configuración general
# =========================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))

os_name <- Sys.info()[["sysname"]]
if (os_name %in% c("Darwin", "Windows")) {
  options(pkgType = "binary")
} else {
  options(pkgType = "source")
}

# =========================================================
# (1) Dependencias declaradas de la app
# =========================================================
.core <- c(
  "here", "shiny", "shinydashboard", "shinydashboardPlus",
  "shinyWidgets", "shinyjs",
  "readxl", "readr", "openxlsx", "dplyr", "tidyr", "janitor",
  "data.table", "stringi", "scales",
  "ggplot2", "highcharter", "reactable", "gt",
  "formattable", "png", "htmltools", "viridisLite",
  "MASS", "fitdistrplus", "forecast", "jfa",
  "RcppRoll", "sunburstR", "d3r",
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
heavy_flag     <- Sys.getenv("APP_HEAVY", "FALSE")

is_bootstrap <- identical(toupper(bootstrap_flag), "TRUE")
use_heavy    <- identical(toupper(heavy_flag), "TRUE")

# =========================================================
# (3) Helpers
# =========================================================
.pkgs_missing <- function(pkgs) {
  setdiff(pkgs, rownames(installed.packages()))
}

.instalar_si_faltan <- function(pkgs, force = FALSE) {
  installed_now <- rownames(installed.packages())
  faltan <- if (force) pkgs else setdiff(pkgs, installed_now)

  if (length(faltan) == 0) {
    message("✅ No hay paquetes pendientes en este bloque.")
    return(invisible(TRUE))
  }

  message("📦 Instalando paquetes: ", paste(faltan, collapse = ", "))
  install.packages(faltan, dependencies = TRUE, quiet = TRUE)
  invisible(TRUE)
}

.verificar_sin_instalar <- function(pkgs, block_name = ".core") {
  faltan <- .pkgs_missing(pkgs)
  if (length(faltan) > 0) {
    stop(
      paste0(
        "Faltan paquetes requeridos en runtime para ", block_name, ": ",
        paste(faltan, collapse = ", "),
        "\nReconstruye la imagen Docker o ejecuta APP_BOOTSTRAP=TRUE antes del runtime."
      ),
      call. = FALSE
    )
  }
  message("✅ Runtime verificado correctamente para ", block_name)
  invisible(TRUE)
}

.cargar_todos <- function(pkgs) {
  invisible(lapply(
    pkgs,
    function(p) {
      suppressPackageStartupMessages(
        library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
    }
  ))
}

# =========================================================
# (4) Bootstrap / Runtime
# =========================================================
if (is_bootstrap) {
  message("🚀 Modo BOOTSTRAP (APP_BOOTSTRAP=TRUE): instalando y cargando paquetes .core")
  .instalar_si_faltan(.core, force = FALSE)
} else {
  message("⚙️  Modo RUNTIME (APP_BOOTSTRAP=FALSE): no se instala nada; solo se verifica .core")
  .verificar_sin_instalar(.core, block_name = ".core")
}

.cargar_todos(.core)

if (use_heavy) {
  if (is_bootstrap) {
    message("💪 APP_HEAVY=TRUE + BOOTSTRAP: instalando y cargando paquetes pesados")
    .instalar_si_faltan(.heavy, force = FALSE)
  } else {
    message("💪 APP_HEAVY=TRUE + RUNTIME: verificando paquetes pesados sin instalar")
    .verificar_sin_instalar(.heavy, block_name = ".heavy")
  }
  .cargar_todos(.heavy)
  message("✅ Paquetes pesados cargados correctamente.")
} else {
  message("ℹ️ APP_HEAVY=FALSE → paquetes pesados omitidos.")
}

# =========================================================
# (5) Información final del entorno
# =========================================================
cat("\n✅ Librerías listas y entorno inicializado correctamente.\n")
if ("here" %in% rownames(installed.packages())) {
  suppressPackageStartupMessages(library(here, quietly = TRUE, warn.conflicts = FALSE))
  cat("📂 Raíz del proyecto detectada por {here}: ", here(), "\n", sep = "")
}
