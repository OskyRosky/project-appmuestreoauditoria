######################################################################
#                                                                    #
#                                                                    #
#                Muestreo proporcional al tamaño (PPT)               #
#                 "Proportional to Size" (PTS)                       #   
#                                                                    #
######################################################################

##############################
#  Método 1 ---> manual
##########################

# La selección proporcional al tamaño (PPT) es una técnica de muestreo en la que la probabilidad 
# de seleccionar cualquier unidad de la población es proporcional a su tamaño. En el contexto del muestreo de 
# unidades monetarias, el "tamaño" a menudo se refiere al valor monetario de una transacción o saldo.

# Para realizar una selección proporcional al tamaño en R, puedes seguir los siguientes pasos:

# 1. Calcular la probabilidad de selección para cada unidad basada en su valor monetario.
# 2. Seleccionar las unidades de la muestra usando un algoritmo adecuado.


# Crear un conjunto de datos de ejemplo con valores monetarios
transacciones <- data.frame(
  id = 1:20,
  valor = c(10, 50, 30, 70, 20, 60, 40, 25, 55, 65,
            1000, 5000, 3000, 7000, 2000, 6000, 4000, 2500, 5500, 6500)
)

# Paso 1: Calcular la probabilidad de selección para cada unidad

# El tamaño total del conjunto es la suma de todos los valores
total_valor <- sum(transacciones$valor)
total_valor

# La probabilidad de selección para cada unidad es proporcional a su valor
transacciones$prob_seleccion <- transacciones$valor / total_valor

probSelec <- transacciones$valor / total_valor
probSelec

# Paso 2: Seleccionar las unidades de la muestra

# El número de unidades a seleccionar
n_muestra <- 5

# Usar la función sample para seleccionar las unidades de acuerdo a su probabilidad
muestra_ids <- sample(transacciones$id, size = n_muestra, replace = FALSE, prob = probSelec)

# Mostrar las transacciones seleccionadas
muestra <- transacciones[transacciones$id %in% muestra_ids, ]
muestra

print(muestra)


#########################################
#    Método 2 ---> librería sampling    #        -----> no sirvió
#########################################

install.packages("survey", dependencies = TRUE)
suppressMessages(library(survey))

# Crear un conjunto de datos de ejemplo con valores monetarios
transacciones2 <- data.frame(
  id = 1:20,
  valor = c(10, 50, 30, 70, 20, 60, 40, 25, 55, 65,
            1000, 5000, 3000, 7000, 2000, 6000, 4000, 2500, 5500, 6500)
)

# Probabilidades proporcionales al tamaño
pik = transacciones2$valor / sum(transacciones2$valor)

# Crear el diseño
design <- svydesign(ids = ~1, probs = ~pik, data = transacciones2)

# Tomar una muestra usando la función base `sample` con las probabilidades de pik
set.seed(123)  # Establecer semilla para reproducibilidad
muestra_ids <- sample(transacciones2$id, size = 8, replace = FALSE, prob = pik)
muestra <- transacciones2[transacciones2$id %in% muestra_ids, ]

print(muestra)
