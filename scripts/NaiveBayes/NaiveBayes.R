# ====================================
# Script de Naive Bayes para el Proyecto
# ====================================

# Sección 1: Cargar librerías, datos y preprocesamiento
library(e1071)    # Para naiveBayes
library(dplyr)    # Para manipulación de datos
library(caret)    # Para evaluación y funciones como RMSE

# Cargar datos preprocesados
train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)

# Asegurar que las variables categóricas de test tengan los mismos niveles que en train
factor_vars <- names(train_data)[sapply(train_data, is.factor)]
for (var in factor_vars) {
  if (var %in% names(test_data)) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}

# Eliminar casos incompletos
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]

# Definir los predictores (se excluye la variable respuesta "SalePrice")
predictors <- setdiff(names(train_data), "SalePrice")


# Sección 2: Modelo de Regresión con Naive Bayes
# Objetivo: Predecir SalesPrice usando Naive Bayes adaptado para regresión

# 2.1. Discretizar la variable respuesta en el conjunto de entrenamiento
n_bins <- 50  # Ajusta este número según convenga
train_data$SalesPrice_bin <- cut(train_data$SalePrice, breaks = n_bins, include.lowest = TRUE)

# 2.2. Calcular los centros de cada intervalo usando los límites de corte
breaks <- attr(train_data$SalesPrice_bin, "breaks")
bin_centers <- (head(breaks, -1) + tail(breaks, -1)) / 2
cat("Centros de cada bin:\n")
print(bin_centers)

# 2.3. Entrenar el modelo Naive Bayes (regresión) usando la variable discretizada
nb_model_reg <- naiveBayes(SalesPrice_bin ~ ., 
                           data = train_data[, c(predictors, "SalesPrice_bin")])

# 2.4. Predecir en el conjunto de prueba: se obtienen las probabilidades para cada bin
nb_pred_probs <- predict(nb_model_reg, newdata = test_data[, predictors], type = "raw")

# 2.5. Calcular la predicción final como el valor esperado (suma de probabilidad*centro)
nb_pred_reg <- apply(nb_pred_probs, 1, function(prob_vec) sum(prob_vec * bin_centers))

# 2.6. Evaluar el modelo de regresión usando RMSE
rmse_nb_reg <- RMSE(nb_pred_reg, test_data$SalePrice)
cat("RMSE del modelo de Naive Bayes (Regresión):", rmse_nb_reg, "\n")


# Sección 3: Modelo de Clasificación con Naive Bayes
# Objetivo: Clasificar las casas en categorías (por ejemplo, Económicas, Intermedias y Caras)

# 3.1. Crear la variable categórica basada en SalesPrice
# Se utilizan los cuartiles para definir los cortes
quantiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75))
train_data$PriceCat <- cut(train_data$SalePrice, 
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                           labels = c("Economicas", "Intermedias", "Caras"))
test_data$PriceCat <- cut(test_data$SalePrice, 
                          breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                          labels = c("Economicas", "Intermedias", "Caras"))

# 3.2. Entrenar el modelo Naive Bayes para clasificación
nb_model_class <- naiveBayes(PriceCat ~ ., 
                             data = train_data[, c(predictors, "PriceCat")])

# 3.3. Predecir la clase en el conjunto de prueba
nb_pred_class <- predict(nb_model_class, newdata = test_data[, predictors])

# 3.4. Evaluar el modelo de clasificación con una matriz de confusión
cm_nb_class <- confusionMatrix(nb_pred_class, test_data$PriceCat)
cat("\nMatriz de Confusión y Métricas del Modelo Naive Bayes (Clasificación):\n")
print(cm_nb_class)

# Fin del script
