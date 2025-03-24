# Cargar librerías necesarias
library(caret)    # Para modelado y validación (train, confusionMatrix, etc.)
library(dplyr)    # Para manipulación de datos (case_when, etc.)
library(ggplot2)  # Para gráficos
library(e1071)    # Para funciones auxiliares, como el modelo Naive Bayes (si fuera necesario)
library(Metrics)  # Para calcular métricas como RMSE, MAE, etc.
library(class)
library(rpart)        # Para árboles de decisión
library(rpart.plot)   # Para visualizar árboles
library(randomForest) # Para Random Forest
library(reshape2)     # Para reorganizar data frames en formato largo (melt)

# Función para el primer código (modelo KNN simple)
  # ====================================================
  # 1. Cargar y preparar los datos
  # ====================================================
  
  train_data <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
  test_data  <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
  
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



#===============================================================
# Sobreajuste
#================================================================
  # Definir el rango de k a evaluar
  k_range <- seq(3, 15, by = 1)
  
  # Inicializar vectores para guardar la precisión en train y test
  train_acc <- numeric(length(k_range))
  test_acc  <- numeric(length(k_range))
  
  # Iterar sobre cada valor de k
  for (i in seq_along(k_range)) {
    k_val <- k_range[i]
    
    # Predicción en el conjunto de entrenamiento (usando train como ambos: datos a clasificar y referencia)
    pred_train <- knn(x_train, x_train, y_train, k = k_val)
    cm_train <- confusionMatrix(pred_train, y_train)
    train_acc[i] <- cm_train$overall["Accuracy"]
    
    # Predicción en el conjunto de prueba
    pred_test <- knn(x_train, x_test, y_train, k = k_val)
    cm_test <- confusionMatrix(pred_test, y_test)
    test_acc[i] <- cm_test$overall["Accuracy"]
  }
  
  # Crear un data frame para la gráfica
  df_accuracy <- data.frame(
    k = rep(k_range, times = 2),
    Accuracy = c(train_acc, test_acc),
    Conjunto = rep(c("Train", "Test"), each = length(k_range))
  )
  
  # Graficar la precisión en función de k para train y test
  ggplot(df_accuracy, aes(x = k, y = Accuracy, color = Conjunto)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(title = "Comparación de Accuracy: Train vs. Test para diferentes valores de k",
         x = "Número de vecinos (k)",
         y = "Accuracy") +
    theme_minimal() +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red"))
  

#================================================================
# Modelo de KNN Validacion Cruzada y Hiperparametros 
#================================================================
  # Cargar datos
  train_data <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
  test_data  <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
  
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
  
  
# ====================================================
# Modelo con Hipermarametros
# ====================================================
  # Un ejemplo:
  predictors <- setdiff(names(train_data), c("SalePrice", "PriceCat"))
  formula_kknn <- as.formula(paste("PriceCat ~", paste(predictors, collapse = " + ")))
  
  # ---------------------------------------------------------
  # 2. Configurar la validación cruzada
  # ---------------------------------------------------------
  tr_control <- trainControl(
    method = "repeatedcv",   # Validación cruzada repetida
    number = 5,             # Número de folds
    repeats = 2,            # Número de repeticiones
    verboseIter = FALSE,    # Para ver menos información en pantalla
    classProbs = TRUE,      # Para calcular probabilidades de clase (si se desea)
    summaryFunction = multiClassSummary  # Métricas multicategoría
  )
  
  # ---------------------------------------------------------
  # 3. Definir la grilla de hiperparámetros a probar
  # ---------------------------------------------------------
  # kmax:  número máximo de vecinos
  # distance: 1=Manhattan, 2=Euclidiana, etc.
  # kernel: ponderación de los vecinos (rectangular, triangular, epanechnikov, optimal, etc.)
  # Ajusta los valores según lo que quieras explorar
  
  tune_grid <- expand.grid(
    kmax     = c(3, 5, 7, 9, 11),
    distance = c(1, 2), 
    kernel   = c("rectangular", "triangular", "epanechnikov")
  )
  
  # ---------------------------------------------------------
  # 4. Entrenar el modelo con caret (método = "kknn")
  # ---------------------------------------------------------
  set.seed(123)  # Para reproducibilidad
  model_kknn <- train(
    formula_kknn,
    data       = train_data,
    method     = "kknn",         # Método de caret que integra kknn
    trControl  = tr_control, 
    tuneGrid   = tune_grid,
    preProcess = c("center","scale"),  # Escalado y centrado (recomendado para KNN)
    metric     = "Accuracy"           # Métrica principal a optimizar
  )
  
  # Ver resultados del tuning
  print(model_kknn)
  cat("Mejor combinación de hiperparámetros:\n")
  print(model_kknn$bestTune)
  
  # ---------------------------------------------------------
  # 5. Predecir en el conjunto de prueba y evaluar
  # ---------------------------------------------------------
  # Asumiendo que test_data está preprocesado de forma similar
  pred_kknn <- predict(model_kknn, newdata = test_data)
  
  # Matriz de confusión
  cm_kknn <- confusionMatrix(pred_kknn, test_data$PriceCat)
  print(cm_kknn)
  
#===============================================================
# Comparación de Modelos de Clasificación
#==============================================================
  
  
  # ------------------------------------------------------------------
  # 1. Definir los predictores
  # ------------------------------------------------------------------
  # No usaremos SalePrice, pues PriceCat se deriva de él.
  predictors <- setdiff(names(train_data), c("SalePrice", "PriceCat"))
  
  # ------------------------------------------------------------------
  # 2. Entrenar los diferentes modelos
  # ------------------------------------------------------------------
  
  # 2.1. Árbol de Clasificación Base (rpart)
  formula_class <- PriceCat ~ . - SalePrice
  modelo_class_base <- rpart(formula_class, data = train_data, method = "class")
  # Predicción en test
  pred_tree_base <- predict(modelo_class_base, newdata = test_data, type = "class")
  cm_tree_base <- confusionMatrix(pred_tree_base, test_data$PriceCat)
  
  # 2.2. Árbol Tuned con Validación Cruzada (rpart2)
  set.seed(123)
  control_cv <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  modelo_class_cv <- train(formula_class, 
                           data = train_data, 
                           method = "rpart2",      
                           tuneGrid = expand.grid(maxdepth = 2:10),
                           trControl = control_cv,
                           metric = "Accuracy")
  pred_tree_cv <- predict(modelo_class_cv, newdata = test_data, type = "raw")
  cm_tree_cv <- confusionMatrix(pred_tree_cv, test_data$PriceCat)
  
  # 2.3. Random Forest para Clasificación
  set.seed(123)
  modelo_rf <- randomForest(PriceCat ~ . - SalePrice, data = train_data, na.action = na.omit)
  pred_rf <- predict(modelo_rf, newdata = test_data)
  cm_rf <- confusionMatrix(pred_rf, test_data$PriceCat)
  
  # 2.4. Naive Bayes para Clasificación
  nb_model_class <- naiveBayes(PriceCat ~ . - SalePrice, data = train_data)
  pred_nb <- predict(nb_model_class, newdata = test_data[, predictors])
  cm_nb <- confusionMatrix(pred_nb, test_data$PriceCat)
  
  # 2.5. KNN para Clasificación (modelo simple)
  # Seleccionar solo las variables numéricas de los predictores
  numeric_predictors <- predictors[sapply(train_data[, predictors], is.numeric)]
  x_train <- scale(train_data[, numeric_predictors])
  x_test  <- scale(test_data[, numeric_predictors])
  y_train <- train_data$PriceCat
  y_test  <- test_data$PriceCat
  # Elegir k como la raíz cuadrada del número de observaciones
  k_simple <- round(sqrt(nrow(train_data)), 0)
  pred_knn <- knn(x_train, x_test, y_train, k = k_simple)
  cm_knn <- confusionMatrix(pred_knn, y_test)
  
  # ------------------------------------------------------------------
  # 3. Función para calcular el F1-score promedio a partir de la matriz de confusión
  # ------------------------------------------------------------------
  calcular_f1_promedio <- function(cm) {
    # En el caso multicategoría, cm$byClass es una matriz
    if (is.matrix(cm$byClass)) {
      # Extraer la columna "F1" y calcular la media
      return(mean(cm$byClass[,"F1"], na.rm = TRUE))
    } else {
      # En caso de tener solo una clase (vector)
      return(cm$byClass["F1"])
    }
  }
  
  # ------------------------------------------------------------------
  # 4. Extraer las métricas de cada modelo
  # ------------------------------------------------------------------
  metrics_df <- data.frame(
    Model = c("Tree Base", "Tree CV", "Random Forest", "Naive Bayes", "KNN"),
    Accuracy = c(cm_tree_base$overall["Accuracy"],
                 cm_tree_cv$overall["Accuracy"],
                 cm_rf$overall["Accuracy"],
                 cm_nb$overall["Accuracy"],
                 cm_knn$overall["Accuracy"]),
    Kappa = c(cm_tree_base$overall["Kappa"],
              cm_tree_cv$overall["Kappa"],
              cm_rf$overall["Kappa"],
              cm_nb$overall["Kappa"],
              cm_knn$overall["Kappa"]),
    F1 = c(calcular_f1_promedio(cm_tree_base),
           calcular_f1_promedio(cm_tree_cv),
           calcular_f1_promedio(cm_rf),
           calcular_f1_promedio(cm_nb),
           calcular_f1_promedio(cm_knn))
  )
  cat("Comparación de métricas de clasificación:\n")
  print(metrics_df)
  
  # ------------------------------------------------------------------
  # 5. Graficar la comparación de Accuracy y F1-score
  # ------------------------------------------------------------------
  metrics_melt <- melt(metrics_df, id.vars = "Model", variable.name = "Metric", value.name = "Value")
  
  ggplot(metrics_melt, aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Comparación de Modelos de Clasificación",
         y = "Valor de la Métrica") +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1")
