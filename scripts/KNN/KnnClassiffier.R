# Cargar librerías necesarias
library(caret)    # Para modelado y validación (train, confusionMatrix, etc.)
library(dplyr)    # Para manipulación de datos (case_when, etc.)
library(ggplot2)  # Para gráficos
library(e1071)    # Para funciones auxiliares, como el modelo Naive Bayes (si fuera necesario)

# ====================================================
# 1. Cargar y preparar los datos
# ====================================================

# Cargar los datos preprocesados para entrenamiento y prueba
train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)

# Ajustar los niveles de las variables categóricas en test_data para que coincidan con los de train_data
factor_vars <- names(train_data)[sapply(train_data, is.factor)]
for (var in factor_vars) {
  if (var %in% names(test_data)) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}

# Eliminar filas que contengan NA en ambos conjuntos para trabajar con datos completos
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]

# Mostrar resumen de la variable respuesta (SalePrice) en el conjunto de entrenamiento
cat("Resumen de SalePrice en train_data:\n")
print(summary(train_data$SalePrice))

# ====================================================
# 2. Crear la variable de clasificación PriceCat
# ====================================================

# Calcular los umbrales basados en el primer (25%) y tercer (75%) cuartil de SalePrice en train_data
cuartiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75), na.rm = TRUE)
lower_threshold <- cuartiles[1]  # Umbral inferior
upper_threshold <- cuartiles[2]  # Umbral superior

cat("Umbral inferior (25%):", lower_threshold, "\n")
cat("Umbral superior (75%):", upper_threshold, "\n")

# Crear la variable PriceCat en train_data utilizando case_when:
# - "Economicas": SalePrice menor que el umbral inferior
# - "Intermedias": SalePrice entre el umbral inferior y el superior
# - "Caras": SalePrice mayor o igual al umbral superior
train_data$PriceCat <- case_when(
  train_data$SalePrice < lower_threshold ~ "Economicas",
  train_data$SalePrice < upper_threshold ~ "Intermedias",
  TRUE ~ "Caras"
)
# Convertir PriceCat a factor con el orden deseado
train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))

# Repetir el mismo proceso para test_data usando los mismos umbrales
test_data$PriceCat <- case_when(
  test_data$SalePrice < lower_threshold ~ "Economicas",
  test_data$SalePrice < upper_threshold ~ "Intermedias",
  TRUE ~ "Caras"
)
test_data$PriceCat <- factor(test_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))

# Mostrar la distribución de PriceCat en ambos conjuntos
cat("Distribución de PriceCat en train_data:\n")
print(table(train_data$PriceCat))
cat("Distribución de PriceCat en test_data:\n")
print(table(test_data$PriceCat))

#================================================================
# Modelo de clasificación K-Nearest Neighbors (KNN)
#================================================================
# ------------------------------------------------------------------
# 1. Seleccionar los predictores numéricos (excluyendo SalePrice y PriceCat)
# ------------------------------------------------------------------
predictors <- setdiff(names(train_data), c("SalePrice", "PriceCat"))
# Seleccionar solo las variables numéricas de los predictores
numeric_predictors <- predictors[sapply(train_data[, predictors], is.numeric)]

# ------------------------------------------------------------------
# 2. Normalizar los datos
# ------------------------------------------------------------------
# Normalizamos los predictores en el conjunto de entrenamiento y prueba
x_train <- scale(train_data[, numeric_predictors])
x_test  <- scale(test_data[, numeric_predictors])
# Definir las etiquetas de clase
y_train <- train_data$PriceCat
y_test  <- test_data$PriceCat

# ------------------------------------------------------------------
# 3. Seleccionar el valor de k
# ------------------------------------------------------------------
# Usamos k igual a la raíz cuadrada del número de observaciones en el conjunto de entrenamiento
k <- round(sqrt(nrow(train_data)), 0)
cat("Valor de k seleccionado:", k, "\n")

# ------------------------------------------------------------------
# 4. Modelo de clasificación KNN
# ------------------------------------------------------------------
# Aplicamos el algoritmo KNN
predModelo1 <- knn(x_train, x_test, y_train, k)

# ------------------------------------------------------------------
# 5. Evaluar el modelo con la matriz de confusión
# ------------------------------------------------------------------
cm <- confusionMatrix(predModelo1, y_test)
print(cm)


