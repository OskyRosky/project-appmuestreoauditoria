#####################################################################
#                         Muestreo financiero                       #
#                    Lanzador principal (runner)                    #
#####################################################################

#####################################################################
#                  AppAuditSample.R  — Lanzador Shiny               #
#                (v1/AppAuditSample.R  + Scripts_dashboard/)        #
#####################################################################

# ----- Opciones generales
options(shiny.maxRequestSize = 100 * 1024^2)   # 100 MB
options(encoding = "UTF-8")
options(scipen   = 999)
set.seed(as.integer(Sys.time()))

# ----- Localiza la carpeta del script (NO depende de {here})
script_dir <- (function() {
  ca <- commandArgs(trailingOnly = FALSE)
  m  <- grep("^--file=", ca)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", ca[m]))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  normalizePath(getwd())
})()

setwd(script_dir)  # ancla paths relativos al directorio del script

scripts_dir <- file.path(script_dir, "Scripts_dashboard")
if (!dir.exists(scripts_dir)) {
  stop("No se encontró la carpeta 'Scripts_dashboard' en: ", scripts_dir,
       "\nVerifica que esté al lado de AppAuditSample.R.")
}
cat("📂 Scripts dir: ", scripts_dir, "\n", sep = "")

# ----- Helper para source en orden y en el GlobalEnv
.cargar <- function(fname) {
  fpath <- file.path(scripts_dir, fname)
  if (!file.exists(fpath)) stop("No existe: ", fpath)
  source(fpath, local = .GlobalEnv, chdir = TRUE, encoding = "UTF-8")
  cat("✅ Cargado: ", fname, "\n", sep = "")
}

# 1) Dependencias (aquí se carga {shiny})
.cargar("Librerias.R")

# ============================================================
#  🎨 Registrar carpeta /www/ima para imágenes estáticas
# ============================================================
www_ima_dir <- file.path(script_dir, "www", "ima")
if (dir.exists(www_ima_dir)) {
  shiny::addResourcePath("ima", www_ima_dir)
  cat("🖼️ Recursos estáticos servidos desde: ", www_ima_dir, "\n", sep = "")
} else {
  cat("⚠️ Advertencia: no se encontró carpeta www/ima en:\n  ",
      www_ima_dir, "\n", sep = "")
}

# 2) Parámetros (si existe)
if (file.exists(file.path(scripts_dir, "Parametros.R"))) .cargar("Parametros.R")

# 3) Partes UI (orden importa)
.cargar("header.R")
.cargar("sider.R")
.cargar("body.R")
.cargar("ui.R")       # define `ui` usando header/sidebar/body

# 4) Lógica de servidor
.cargar("server.R")   # define `server`

# Sanidad mínima
stopifnot(exists("ui",    inherits = TRUE))
stopifnot(exists("server",inherits = TRUE))

# ----- Host/Port (se pueden sobreescribir con variables de entorno)
APP_HOST <- Sys.getenv("APP_HOST", unset = "0.0.0.0")
APP_PORT <- as.integer(Sys.getenv("APP_PORT", unset = "8000"))

# Info útil de acceso en LAN (best-effort)
.show_ip <- function() {
  ip <- NA_character_
  os <- Sys.info()[["sysname"]]
  try({
    if (identical(os, "Darwin")) {
      ip <- system2("ipconfig", c("getifaddr", "en0"), stdout = TRUE, stderr = FALSE)
      if (!length(ip) || is.na(ip) || !nzchar(ip)) {
        ip <- system2("ipconfig", c("getifaddr", "en1"), stdout = TRUE, stderr = FALSE)
      }
    } else if (identical(os, "Linux")) {
      ip <- system("hostname -I", intern = TRUE)
      ip <- strsplit(ip, "\\s+")[[1]][1]
    } else if (grepl("Windows", os, TRUE)) {
      ln <- system("ipconfig", intern = TRUE)
      l4 <- grep("IPv4", ln, value = TRUE)
      if (length(l4)) ip <- sub(".*?:\\s*", "", l4[1])
    }
  }, silent = TRUE)
  if (length(ip) && !is.na(ip) && nzchar(ip)) {
    cat("🌐 Acceso en red local: http://", ip, ":", APP_PORT, "/\n", sep = "")
  }
}
.show_ip()

cat("🚀 Iniciando Shiny en ", APP_HOST, ":", APP_PORT, " …\n", sep = "")

# ----- Ejecutar
shiny::runApp(
  list(ui = ui, server = server),
  host = APP_HOST,
  port = APP_PORT,
  launch.browser = interactive()
)