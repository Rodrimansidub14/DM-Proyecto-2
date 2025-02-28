# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
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
# Supongamos que ya tienes cargado el dataset en 'datos'
# 1. Seleccionar las variables numéricas
datos_num <- datos %>% select_if(is.numeric)

# 2. Opcional: reducir el número de variables
#    Por ejemplo, elegimos algunas de interés o las más correlacionadas con SalePrice
#    Aquí muestro un ejemplo con variables típicas:
vars_interes <- c("SalePrice", "GrLivArea", "LotFrontage", "LotArea", 
                  "OverallQual", "YearBuilt", "GarageCars")

# Filtramos el data frame para quedarnos solo con esas columnas
datos_sub <- datos_num[, vars_interes]

# 3. Generar la matriz de gráficos con ggpairs
#    Esto mostrará histogramas en la diagonal, scatter plots en la parte superior/inferior, 
#    y la correlación entre variables.
ggpairs(datos_num,
        title = "Matriz de Gráficos con ggpairs (subset de variables)",
        upper = list(continuous = wrap("cor", size = 4)),  # Muestra correlación en la parte superior
        lower = list(continuous = "smooth"),               # Ajusta una línea de regresión
        diag = list(continuous = "barDiag"))               # Histogramas en la diagonal


datos_num_long <- datos_num %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valor")

ggplot(datos_num_long, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Histogramas de Variables Numéricas",
       x = "Valor",
       y = "Frecuencia")

pr


ggplot(datos_num_long, aes(y = valor)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots de Variables Numéricas",
       y = "Valor")





