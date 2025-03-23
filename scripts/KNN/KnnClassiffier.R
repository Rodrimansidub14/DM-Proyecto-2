# Cargar librerías necesarias
library(caret)    # Para modelado y validación (train, confusionMatrix, etc.)
library(dplyr)    # Para manipulación de datos (case_when, etc.)
library(ggplot2)  # Para gráficos
library(e1071)    # Para funciones auxiliares, como el modelo Naive Bayes (si fuera necesario)
library(Metrics)  # Para calcular métricas como RMSE, MAE, etc.

# Función para el primer código (modelo KNN simple)
modelo_knn_simple <- function() {
  # ====================================================
  # 1. Cargar y preparar los datos
  # ====================================================
  
  train_data <- read.csv("train_preprocessed.csv", stringsAsFactors = TRUE)
  test_data  <- read.csv("test_preprocessed.csv", stringsAsFactors = TRUE)
  
  # Ajustar los niveles de las variables categóricas
  factor_vars <- names(train_data)[sapply(train_data, is.factor)]
  for (var in factor_vars) {
    if (var %in% names(test_data)) {
      test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
    }
  }
  
  # Eliminar filas con NA
  train_data <- train_data[complete.cases(train_data), ]
  test_data  <- test_data[complete.cases(test_data), ]
  
# ====================================================
# 2. Crear la variable de clasificación PriceCat
# ====================================================
  
  cuartiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75), na.rm = TRUE)
  lower_threshold <- cuartiles[1]
  upper_threshold <- cuartiles[2]
  
  train_data$PriceCat <- case_when(
    train_data$SalePrice < lower_threshold ~ "Economicas",
    train_data$SalePrice < upper_threshold ~ "Intermedias",
    TRUE ~ "Caras"
  )
  train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))
  
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
}

#================================================================
# Modelo de KNN Validacion Cruzada
#================================================================
modelo_knn_cv <- function() {
  # Cargar datos
  train_data <- read.csv("train_preprocessed.csv", stringsAsFactors = TRUE)
  test_data  <- read.csv("test_preprocessed.csv", stringsAsFactors = TRUE)
  
  # Eliminar variables con varianza cero o casi cero
  cols_to_remove <- nearZeroVar(train_data)
  if (length(cols_to_remove) > 0) {
    train_data <- train_data[, -cols_to_remove, drop = FALSE]
    test_data  <- test_data[, -cols_to_remove, drop = FALSE]
  }
  
  # Verificación manual de columnas con un solo valor
  zero_var_cols <- sapply(train_data, function(x) length(unique(x)) == 1)
  cols_to_remove_manual <- names(train_data)[zero_var_cols]
  if (length(cols_to_remove_manual) > 0) {
    train_data <- train_data[, !(names(train_data) %in% cols_to_remove_manual), drop = FALSE]
    test_data  <- test_data[, !(names(test_data) %in% cols_to_remove_manual), drop = FALSE]
  }
  
  # Igualar niveles de factores en test_data
  factor_cols <- names(train_data)[sapply(train_data, is.factor)]
  for (col in factor_cols) {
    test_data[[col]] <- factor(test_data[[col]], levels = levels(train_data[[col]]))
    if (any(is.na(test_data[[col]]))) {
      mode_val <- names(sort(table(train_data[[col]]), decreasing = TRUE))[1]
      test_data[[col]][is.na(test_data[[col]])] <- mode_val
    }
  }
  
  numeric_cols <- setdiff(names(train_data)[sapply(train_data, is.numeric)], "SalePrice")
  for (col in numeric_cols) {
    train_data[[col]][is.na(train_data[[col]])] <- median(train_data[[col]], na.rm = TRUE)
    test_data[[col]][is.na(test_data[[col]])] <- median(train_data[[col]], na.rm = TRUE)
  }
  
  # Eliminar filas con valores NA
  train_data <- train_data[complete.cases(train_data), ]
  test_data  <- test_data[complete.cases(test_data), ]
  
  # Configuración de validación cruzada con 10 pliegues
  train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
  
  # Entrenar el modelo KNN con validación cruzada
  set.seed(123)
  knn_cv <- train(SalePrice ~ ., 
                  data = train_data, 
                  method = "knn", 
                  preProcess = c("center", "scale"),
                  trControl = train_control)
  
  pred_cv <- predict(knn_cv, newdata = test_data)
  
  # Calcular métricas para el modelo con validación cruzada
  rmse_cv <- rmse(test_data$SalePrice, pred_cv)
  mae_cv  <- mae(test_data$SalePrice, pred_cv)
  r2_cv   <- 1 - sum((test_data$SalePrice - pred_cv)^2) / sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
  mse_cv  <- mean((test_data$SalePrice - pred_cv)^2) 
  
  # Imprimir métricas finales
  cat("KNN con Validación Cruzada - RMSE: ", rmse_cv, "\n")
  cat("KNN con Validación Cruzada - MAE: ", mae_cv, "\n")
  cat("KNN con Validación Cruzada - MSE: ", mse_cv, "\n") 
  cat("KNN con Validación Cruzada - R²: ", r2_cv, "\n")
}

modelo_knn_simple()   
modelo_knn_cv() 