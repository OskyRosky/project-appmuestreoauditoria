# 1) Paquetes
library(officer)
library(flextable)

# 2) Datos de prueba
Stats <- data.frame(Medida=c("A","B"), Valor=c(1.1,2.2))

# ---------- Opción A: con flextable ----------
docA <- read_docx()
ft   <- flextable(Stats)
docA <- flextable::body_add_flextable(docA, value = ft)

outA <- file.path(Sys.getenv("HOME"), "Desktop",
                  paste0("test_officer_flextable_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx"))
print(docA, target = outA)
cat("Guardado:", outA, "\n")
system2("open", outA)  # abre el archivo en macOS

# ---------- Opción B: solo officer ----------
docB <- read_docx()
docB <- officer::body_add_table(docB, value = Stats, style = "table_template") # si no tienes ese estilo, igual se guarda con el default

outB <- file.path(Sys.getenv("HOME"), "Desktop",
                  paste0("test_officer_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx"))
print(docB, target = outB)
cat("Guardado:", outB, "\n")
system2("open", outB)