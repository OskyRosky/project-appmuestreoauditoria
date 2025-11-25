###############################################################
# ⚙️ Parámetros globales del proyecto
# -------------------------------------------------------------
# Este script define constantes y variables temporales usadas
# en todo el Dashboard de Muestreo de Auditoría.
#
# Contiene:
#   • Escalas base (por ejemplo, millones).
#   • Fechas de referencia (año, mes actual y anteriores).
#   • Cálculo automático de períodos relativos.
###############################################################

# =============================================================
# (1) Escala base para valores monetarios
# -------------------------------------------------------------
# Usada para convertir cifras a millones y mantener consistencia
# en reportes, gráficos y tablas.
# =============================================================
MILLONES <- 1e6  # equivalente a 1,000,000

# =============================================================
# (2) Año inicial de análisis
# -------------------------------------------------------------
# Define desde qué año se analizan los datos (configurable).
# Este valor puede modificarse según la versión del estudio.
# =============================================================
ANIO_ANALISIS_INICIAL <- 2007

# =============================================================
# (3) Años de referencia (actual y dos previos)
# -------------------------------------------------------------
# Se calculan dinámicamente a partir de la fecha del sistema.
# Esto permite que los reportes estén siempre actualizados.
# =============================================================
FECHA_HOY    <- Sys.Date()
ANIO_ACTUAL  <- as.integer(format(FECHA_HOY, "%Y"))
ANIO_PREVIO1 <- ANIO_ACTUAL - 1
ANIO_PREVIO2 <- ANIO_ACTUAL - 2

# =============================================================
# (4) Mes de referencia (numérico y texto)
# -------------------------------------------------------------
# Dos representaciones:
#   • MES_NUM: número de mes (1–12)
#   • MES_STR: texto de dos dígitos ("01"–"12")
# =============================================================
MES_NUM <- as.integer(format(FECHA_HOY, "%m"))
MES_STR <- format(FECHA_HOY, "%m")

# =============================================================
# (5) Meses faltantes para completar el año
# -------------------------------------------------------------
# Permite determinar el avance o proporción de ejecución anual.
# =============================================================
MESES_FALTANTES <- 12 - MES_NUM

# =============================================================
# (6) Mensaje informativo (opcional)
# -------------------------------------------------------------
# Solo informativo, útil en logs o al iniciar la app.
# =============================================================
cat("📅 Fecha actual:", FECHA_HOY, "\n")
cat("🗓️  Año actual:", ANIO_ACTUAL, "| Mes actual:", MES_STR, "\n")
cat("📆  Faltan", MESES_FALTANTES, "mes(es) para finalizar el año.\n")