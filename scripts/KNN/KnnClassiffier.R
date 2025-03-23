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
  
  train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
  test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)
  
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
# Modelo de KNN Validacion Cruzada y Hiperparametros 
#================================================================
modelo_knn_cv_tuning <- function() {
  # Cargar datos
  train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
  test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)
  
  # Eliminar variables con varianza cero en train_data
  cols_to_remove <- nearZeroVar(train_data)
  if (length(cols_to_remove) > 0) {
    train_data <- train_data[, -cols_to_remove, drop = FALSE]
    
    # Asegurar que test_data también elimine las mismas columnas
    common_cols <- intersect(names(train_data), names(test_data))
    test_data <- test_data[, common_cols, drop = FALSE]
  }
  
  # Manejo de valores NA en datos numéricos
  numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
  for (col in numeric_cols) {
    train_data[[col]][is.na(train_data[[col]])] <- median(train_data[[col]], na.rm = TRUE)
    test_data[[col]][is.na(test_data[[col]])] <- median(train_data[[col]], na.rm = TRUE)  # Usar la mediana de train
  }

  factor_cols <- names(train_data)[sapply(train_data, is.factor)]
  for (col in factor_cols) {
    mode_value <- names(sort(table(train_data[[col]]), decreasing = TRUE))[1]
    train_data[[col]][is.na(train_data[[col]])] <- mode_value
    test_data[[col]][is.na(test_data[[col]])] <- mode_value
  }
  
  # Crear la variable de clasificación PriceCat
  cuartiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75), na.rm = TRUE)
  lower_threshold <- cuartiles[1]
  upper_threshold <- cuartiles[2]
  
  train_data$PriceCat <- factor(case_when(
    train_data$SalePrice < lower_threshold ~ "Economicas",
    train_data$SalePrice < upper_threshold ~ "Intermedias",
    TRUE ~ "Caras"
  ), levels = c("Economicas", "Intermedias", "Caras"))
  
  test_data$PriceCat <- factor(case_when(
    test_data$SalePrice < lower_threshold ~ "Economicas",
    test_data$SalePrice < upper_threshold ~ "Intermedias",
    TRUE ~ "Caras"
  ), levels = c("Economicas", "Intermedias", "Caras"))
  
  # Asegurar que test_data y train_data tienen las mismas columnas
  common_cols <- intersect(names(train_data), names(test_data))
  train_data <- train_data[, common_cols, drop = FALSE]
  test_data <- test_data[, common_cols, drop = FALSE]
  
  # Configuración de validación cruzada con 10 pliegues
  train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
  
  # Optimización del hiperparámetro k
  tuneGrid <- expand.grid(k = seq(1, 30, by = 2))  # Prueba valores de k entre 1 y 30
  
  set.seed(123)
  knn_cv_tuned <- train(PriceCat ~ ., 
                         data = train_data, 
                         method = "knn", 
                         preProcess = c("center", "scale"), 
                         trControl = train_control, 
                         tuneGrid = tuneGrid)
  
  # Mejor valor de k encontrado
  best_k <- knn_cv_tuned$bestTune$k
  cat("Mejor valor de k encontrado:", best_k, "\n")
  
  # Predicción con el mejor modelo
  pred_tuned <- predict(knn_cv_tuned, newdata = test_data)
  
  if (length(pred_tuned) != length(test_data$PriceCat)) {
    cat("Advertencia: Diferencia en longitud entre predicciones y datos de prueba\n")
    test_data <- test_data[1:length(pred_tuned), ]
  }
  
  # Evaluación del modelo optimizado
  cm_tuned <- confusionMatrix(pred_tuned, test_data$PriceCat)
  print(cm_tuned)
  
  # Métricas adicionales
  cat("Accuracy:", cm_tuned$overall['Accuracy'], "\n")
  cat("Kappa:", cm_tuned$overall['Kappa'], "\n")
  
  # Calcular F1-score promedio
  precision <- cm_tuned$byClass[, "Precision"]
  recall <- cm_tuned$byClass[, "Recall"]
  
  valid_indices <- !is.na(precision) & !is.na(recall)
  f1_scores <- ifelse(valid_indices, 2 * (precision * recall) / (precision + recall), NA)
  
  f1_mean <- mean(f1_scores, na.rm = TRUE)
  cat("F1-score promedio:", f1_mean, "\n")
}

modelo_knn_simple()   
modelo_knn_cv_tuning()