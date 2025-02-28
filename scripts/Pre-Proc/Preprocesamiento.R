# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

# Cargar el dataset (ajusta la ruta si es necesario)
datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)

# 1. Revisión de la Estructura y Estadísticas Básicas
cat("Cantidad de filas:", nrow(datos), "\n")
cat("Cantidad de columnas:", ncol(datos), "\n")
cat("\nEstructura del dataset:\n")
str(datos)

cat("\nPrimeras 3 filas:\n")
print(head(datos, 3))

cat("\nÚltimas 3 filas:\n")
print(tail(datos, 3))

cat("\nVisualización de las primeras 5 columnas:\n")
print(head(datos[, 1:5]))

# 2. Análisis de Valores Faltantes
faltantes <- sapply(datos, function(x) sum(is.na(x)))
cat("\nCantidad de valores faltantes por variable:\n")
print(faltantes)

missing_percent <- sapply(datos, function(x) sum(is.na(x)) / nrow(datos))
cat("\nPorcentaje de valores faltantes por variable:\n")
print(missing_percent)

# 3. Identificación de Registros Duplicados
duplicados <- datos[duplicated(datos), ]
cat("\nRegistros duplicados:\n")
print(duplicados)

# 4. Detección Preliminar de Outliers
# Función para detectar índices de outliers usando el método IQR
detectar_outliers <- function(x) {
  if (is.numeric(x)) {
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    li <- q[1] - 1.5 * iqr
    ls <- q[2] + 1.5 * iqr
    return(which(x < li | x > ls))
  } else {
    return(NULL)
  }
}

# Selecciona algunas variables clave para revisar outliers (puedes ajustar la selección)
vars_outliers <- c("MSSubClass", "LotFrontage", "LotArea", "GrLivArea", "SalePrice")
for (var in vars_outliers) {
  if (var %in% names(datos)) {
    indices <- detectar_outliers(datos[[var]])
    cat(paste("\nNúmero de outliers en", var, ":", length(indices), "\n"))
    if (length(indices) > 0) {
      cat("Índices de outliers en", var, ":", indices, "\n")
    }
  }
}

# 5. Visualizaciones Básicas

# Histograma y Boxplot para algunas variables numéricas clave
ggplot(datos, aes(x = LotFrontage)) + 
  geom_histogram(fill = "blue", bins = 30, na.rm = TRUE) +
  labs(title = "Histograma de LotFrontage", x = "LotFrontage", y = "Frecuencia")

ggplot(datos, aes(y = LotFrontage)) +
  geom_boxplot(fill = "orange", na.rm = TRUE) +
  labs(title = "Boxplot de LotFrontage", y = "LotFrontage")

ggplot(datos, aes(x = SalePrice)) + 
  geom_histogram(fill = "green", bins = 30, na.rm = TRUE) +
  labs(title = "Histograma de SalePrice", x = "SalePrice", y = "Frecuencia")

# Matriz de correlaciones para variables numéricas
numericas <- sapply(datos, is.numeric)
cor_matrix <- cor(datos[, numericas], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, 
         title = "Matriz de Correlaciones", mar = c(0,0,1,0))

# Diagrama de dispersión entre algunas variables clave
pairs(~LotFrontage + LotArea + GrLivArea + SalePrice + , data = datos, 
      main = "Relación entre variables clave")
