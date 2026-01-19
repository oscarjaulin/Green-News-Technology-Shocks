# Clear the memory
rm(list=ls())

# Cargar librerías necesarias
library(readxl)  # Para leer Excel
library(dplyr)   # Para manipulación de datos

# Función para calcular correlación con test de significancia entre dos vectores
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  return(list(correlation = test$estimate, p_value = test$p.value))
}

# Función para aplicar rezagos o adelantos a un vector
lag_lead <- function(vec, shift = 0) {
  if (shift > 0) {
    # Rezago: desplaza hacia adelante, rellena con NA al final
    return(c(rep(NA, shift), vec[1:(length(vec) - shift)]))
  } else if (shift < 0) {
    # Adelanto: desplaza hacia atrás, rellena con NA al inicio
    shift <- abs(shift)
    return(c(vec[(shift + 1):length(vec)], rep(NA, shift)))
  } else {
    return(vec)
  }
}

# Cargar datos desde Excel (ajusta la ruta y hoja si es necesario)
#Read the macroeconomic Data
setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/Green News Tech shocks/Input/')

datos <- read_excel("Shocks.xlsx", sheet = 1)

# Seleccionar las dos primeras columnas como variables base
var1 <- datos[[2]]  # inst1
var2 <- datos[[3]]  # inst2

# Definir el desplazamiento deseado (lag o lead)
# Por ejemplo, lag = 1 para rezagar 1 periodo, lag = -1 para adelantar 1 periodo
lag_shift <- 0

# Aplicar desplazamiento a las variables base
var1_shifted <- lag_lead(var1, lag_shift)
var2_shifted <- lag_lead(var2, lag_shift)

# Inicializar resultados
resultados <- data.frame(variable = character(),
                         corr_var1 = numeric(),
                         p_val_var1 = numeric(),
                         corr_var2 = numeric(),
                         p_val_var2 = numeric(),
                         stringsAsFactors = FALSE)

# Iterar sobre el resto de columnas para calcular correlaciones y tests
for (i in 4:ncol(datos)) {
  var_i <- datos[[i]]
  
  # Eliminar casos con NA para cada comparación
  valid_idx1 <- complete.cases(var1_shifted, var_i)
  valid_idx2 <- complete.cases(var2_shifted, var_i)
  
  # Calcular test para var1
  test1 <- cor_test(var1_shifted[valid_idx1], var_i[valid_idx1])
  # Calcular test para var2
  test2 <- cor_test(var2_shifted[valid_idx2], var_i[valid_idx2])
  
  # Guardar resultados
  resultados <- rbind(resultados, data.frame(
    variable = colnames(datos)[i],
    corr_var1 = test1$correlation,
    p_val_var1 = test1$p_value,
    corr_var2 = test2$correlation,
    p_val_var2 = test2$p_value,
    stringsAsFactors = FALSE
  ))
}

# Mostrar resultados
print(resultados)
