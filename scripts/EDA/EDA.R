# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)



# Cargar el dataset (ajusta la ruta si es necesario)
datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)

# 1. Revisión de la Estructura y Estadísticas Básicas
cat("Número de filas:", nrow(datos), "\n")
cat("Número de columnas:", ncol(datos), "\n")


str(datos)

cat("\nPrimeras 3 filas:\n")
print(head(datos, 6))

cat("\nÚltimas 3 filas:\n")
print(tail(datos, 6))

cat("\nVisualización de las primeras 5 columnas:\n")
print(head(datos[, 1:5]))


# 2. Análisis descriptivo

library(knitr)

# Seleccionar las columnas numéricas del dataset 'datos'
numeric_cols <- datos[sapply(datos, is.numeric)]

# Función para calcular las estadísticas descriptivas
calc_stats <- function(x) {
  c(
    count = sum(!is.na(x)),
    mean  = mean(x, na.rm = TRUE),
    std   = sd(x, na.rm = TRUE),
    min   = min(x, na.rm = TRUE),
    "25%" = quantile(x, 0.25, na.rm = TRUE),
    "50%" = quantile(x, 0.50, na.rm = TRUE),  # Este valor es la mediana
    "75%" = quantile(x, 0.75, na.rm = TRUE),
    max   = max(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE)
  )
}

# Aplicar la función a cada columna numérica y transponer el resultado
resumen_estadistico <- t(sapply(numeric_cols, calc_stats))
resumen_estadistico <- as.data.frame(resumen_estadistico)

# Redondear los valores para mejorar la legibilidad
resumen_estadistico[] <- lapply(resumen_estadistico, function(x) round(x, 2))

# Mostrar el resumen estadístico con kable
cat("Resumen estadístico de variables numéricas:\n\n")
kable(resumen_estadistico, caption = "Resumen Estadístico de Variables Numéricas")



# Variables categóricas



# Seleccionar las variables categóricas: asumimos que son de tipo character o factor
vars_categoricas <- names(datos)[sapply(datos, function(x) is.character(x) || is.factor(x))]

# Imprimir un mensaje para indicar el inicio del análisis general de variables categóricas
cat("Análisis general de variables categóricas\n")
cat("=========================================\n\n")

# Loop para cada variable categórica
for (var in vars_categoricas) {
  cat("Analizando variable:", var, "\n")
  
  # Generar tabla de frecuencias
  freq_table <- table(datos[[var]])
  print(freq_table)
  
  # Gráfico de barras de la distribución de frecuencias
  p1 <- ggplot(datos, aes_string(x = var)) +
    geom_bar(fill = "steelblue") +
    labs(title = paste("Frecuencia de", var),
         x = var,
         y = "Conteo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p1)
  
  # Si existe la variable objetivo SalePrice, generar boxplot para analizar la relación
  if ("SalePrice" %in% names(datos)) {
    p2 <- ggplot(datos, aes_string(x = var, y = "SalePrice")) +
      geom_boxplot(outlier.colour = "red", fill = "lightblue") +
      labs(title = paste("Distribución de SalePrice por", var),
           x = var,
           y = "SalePrice") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p2)
  }
  
  cat("\n-----------------------------------------\n\n")
}

# Ejemplo de análisis de relaciones cruzadas entre dos variables categóricas:
# Aquí se usa "Neighborhood" y "HouseStyle" como ejemplo. Asegúrate de que existan en tu dataset.
if (all(c("Neighborhood", "HouseStyle") %in% names(datos))) {
  cat("Tabla de contingencia entre Neighborhood y HouseStyle:\n")
  tabla_cruzada <- table(datos$Neighborhood, datos$HouseStyle)
  print(tabla_cruzada)
  
  # Generar un mosaic plot para visualizar la relación
  mosaicplot(tabla_cruzada, main = "Mosaic Plot: Neighborhood vs HouseStyle",
             color = TRUE, las = 2)
}
# ------------------------------------------------------
# SCRIPT DE IDENTIFICACIÓN DE PROBLEMAS EN EL DATASET
# ------------------------------------------------------

# Cargar librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Leer el dataset (ajusta la ruta según corresponda)
datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)

# 1. IDENTIFICACIÓN DE VALORES FALTANTES Y ERRORES DE CODIFICACIÓN
cat("### Análisis de Valores Faltantes y Errores de Codificación ###\n\n")

# Variables de interés: LotFrontage, Alley, PoolQC, Fence, MiscFeature
vars_missing <- c("LotFrontage", "Alley", "PoolQC", "Fence", "MiscFeature")

for (var in vars_missing) {
  cat("Variable:", var, "\n")
  
  # Calcular el número y porcentaje de valores faltantes
  missing_count <- sum(is.na(datos[[var]]))
  missing_percent <- (missing_count / nrow(datos)) * 100
  cat("  Valores faltantes:", missing_count, "(", round(missing_percent, 2), "% )\n")
  
  # Mostrar los valores únicos para detectar posibles errores o codificaciones extrañas
  unique_vals <- unique(datos[[var]])
  cat("  Valores únicos:", paste(unique_vals, collapse = ", "), "\n\n")
}

# 2. IDENTIFICACIÓN DE OUTLIERS Y NECESIDAD DE TRANSFORMACIÓN
cat("### Análisis de Outliers y Posibles Transformaciones ###\n\n")

# Variables de interés para evaluar outliers y transformaciones
vars_outlier <- c("SalePrice", "GrLivArea", "LotArea", "X1stFlrSF", "TotalBsmtSF", "MasVnrArea", "GarageArea")

for (var in vars_outlier) {
  cat("Variable:", var, "\n")
  
  # Mostrar resumen estadístico básico
  print(summary(datos[[var]]))
  
  # Calcular cuantiles importantes
  quantiles <- quantile(datos[[var]], probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99), na.rm = TRUE)
  cat("  Cuantiles (1%, 5%, 25%, 50%, 75%, 95%, 99%):\n")
  print(quantiles)
  
  # Graficar histograma para visualizar la distribución
  p_hist <- ggplot(datos, aes_string(x = var)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
    theme_minimal()
  print(p_hist)
  
  # Graficar boxplot para detectar outliers
  p_box <- ggplot(datos, aes_string(y = var)) +
    geom_boxplot(fill = "orange", outlier.colour = "red", outlier.shape = 16) +
    labs(title = paste("Boxplot de", var), y = var) +
    theme_minimal()
  print(p_box)
  
  cat("\n-----------------------------------------\n\n")
}
