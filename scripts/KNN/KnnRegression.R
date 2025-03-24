# Cargar librerías necesarias
library(caret)
library(Metrics)
library(FNN)
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyverse)

# Cargar datos preprocesados
train_data <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)

# ------------------------------------------------------------------
#  Verificar estructura, resumen y valores faltantes
# ------------------------------------------------------------------
cat("Estructura del conjunto de entrenamiento:\n")
str(train_data)
cat("\nResumen del conjunto de entrenamiento:\n")
print(summary(train_data))
cat("\nValores faltantes en el conjunto de entrenamiento:\n")
print(colSums(is.na(train_data)))

cat("\nEstructura del conjunto de prueba:\n")
str(test_data)
cat("\nResumen del conjunto de prueba:\n")
print(summary(test_data))
cat("\nValores faltantes en el conjunto de prueba:\n")
print(colSums(is.na(test_data)))

# ------------------------------------------------------------------
#  Eliminar variables con varianza cero (o casi cero) usando nearZeroVar
# ------------------------------------------------------------------
nzv_info <- nearZeroVar(train_data, saveMetrics = TRUE)
cols_to_remove <- rownames(nzv_info)[nzv_info$nzv == TRUE]
cat("\nVariables con varianza cero o casi cero:\n")
print(cols_to_remove)

# Remover esas columnas de ambos conjuntos
train_data_clean <- train_data[, !(names(train_data) %in% cols_to_remove)]
test_data_clean  <- test_data[, !(names(test_data) %in% cols_to_remove)]

# ------------------------------------------------------------------
# Asegurar que las variables factor del conjunto de prueba tengan los mismos niveles que en entrenamiento
# ------------------------------------------------------------------
factor_cols <- names(train_data_clean)[sapply(train_data_clean, is.factor)]
for(col in factor_cols) {
  test_data_clean[[col]] <- factor(test_data_clean[[col]], levels = levels(train_data_clean[[col]]))
  # Si aparecen NA (debido a niveles nuevos), se reemplazan por el nivel más frecuente en el entrenamiento
  if(any(is.na(test_data_clean[[col]]))) {
    mode_val <- names(sort(table(train_data_clean[[col]]), decreasing = TRUE))[1]
    test_data_clean[[col]][is.na(test_data_clean[[col]])] <- mode_val
  }
}

# ------------------------------------------------------------------
#  Imputar valores faltantes en el conjunto de prueba para variables numéricas (excepto la respuesta)
# ------------------------------------------------------------------
numeric_cols <- names(test_data_clean)[sapply(test_data_clean, is.numeric)]
# Excluir la variable respuesta "SalePrice" (si ya viene con valor para evaluación)
numeric_cols <- setdiff(numeric_cols, "SalePrice")

for(col in numeric_cols) {
  # Reemplazar NA por la mediana de cada columna
  test_data_clean[[col]][is.na(test_data_clean[[col]])] <- median(test_data_clean[[col]], na.rm = TRUE)
}

# Comprobar que ahora el conjunto de prueba no tenga NA's en variables numéricas
cat("\nValores faltantes tras imputación en test_data_clean:\n")
print(colSums(is.na(test_data_clean)))

# ------------------------------------------------------------------
# 1. Entrenar modelo de regresión KNN
# ------------------------------------------------------------------
# Seleccionar solo filas completas en el entrenamiento
train_complete <- train_data_clean[complete.cases(train_data_clean), ]

# Calcular el valor de k (se usa la raíz cuadrada del número de observaciones)
k_value <- round(sqrt(nrow(train_data_clean)), 0)
params <- expand.grid(k = 9)

set.seed(123)
knn_reg1 <- train(SalePrice ~ ., 
                  data = train_complete, 
                  method = "knn", 
                  preProcess = c("center", "scale", "knnImpute"), 
                  tuneGrid = params)

# ------------------------------------------------------------------
#  Predecir en el conjunto de prueba limpio (ya sin NA's)
# ------------------------------------------------------------------
pred_knn_reg1 <- predict(knn_reg1, newdata = test_data_clean)

# Verificar que la longitud de las predicciones coincida con el número de filas de test_data_clean
cat("Número de filas en test_data_clean:", nrow(test_data_clean), "\n")
cat("Número de predicciones:", length(pred_knn_reg1), "\n")

# Crear un data.frame de resultados para comparar valores reales y predichos
df_result <- data.frame(Index = 1:nrow(test_data_clean),
                        Real = test_data_clean$SalePrice,
                        Pred = pred_knn_reg1)

# Graficar la comparación
ggplot(df_result, aes(x = Index)) +
  geom_point(aes(y = Real, color = "Real"), size = 2) +
  geom_point(aes(y = Pred, color = "Pred"), size = 2) +
  scale_color_manual(values = c("Real" = "blue", "Pred" = "red")) +
  labs(title = "Comparación de valores reales y predichos",
       x = "Índice", y = "Precio de venta", color = "Serie") +
  theme_minimal()

# ------------------------------------------------------------------
#  Evaluación del modelo
# ------------------------------------------------------------------
rmse_value <- rmse(test_data_clean$SalePrice, pred_knn_reg1)
mae_value  <- mae(test_data_clean$SalePrice, pred_knn_reg1)
r2_value   <- 1 - sum((test_data_clean$SalePrice - pred_knn_reg1)^2) / sum((test_data_clean$SalePrice - mean(test_data_clean$SalePrice))^2)

cat("\nResultados del modelo de regresión KNN (k =", k_value, "):\n")
cat("RMSE:", round(rmse_value, 2), "\n")
cat("MAE:", round(mae_value, 2), "\n")
cat("R-squared:", round(r2_value, 2), "\n")



# ------------------------------------------------------------------
#2. Modelo con validación cruzada
# ------------------------------------------------------------------

trainknn_cv <- trainControl(method = "repeatedcv", 
                         number = 10, 
                         repeats = 2)
knn_reg2 <- train(SalePrice ~ ., 
                  data = train_complete, 
                  method = "knn", 
                  preProcess = c("center", "scale"), 
                  trControl = trainknn_cv,
                  tuneGrid = params)
                  
pred_knn_reg2 <- predict(knn_reg2, newdata = test_data_clean)


df_result2 <- data.frame(Index = 1:nrow(test_data_clean),
                        Real = test_data_clean$SalePrice,
                        Pred = pred_knn_reg2)

ggplot(df_result2, aes(x = Index)) +
  geom_point(aes(y = Real, color = "Real"), size = 2) +
  geom_point(aes(y = Pred, color = "Pred"), size = 2) +
  scale_color_manual(values = c("Real" = "blue", "Pred" = "red")) +
  labs(title = "Comparación de valores reales y predichos",
       x = "Índice", y = "Precio de venta", color = "Serie") +
  theme_minimal()



# ------------------------------------------------------------------
# Evaluación del modelo
# ------------------------------------------------------------------
rmse_value2 <- rmse(test_data_clean$SalePrice, pred_knn_reg2)
mae_value2  <- mae(test_data_clean$SalePrice, pred_knn_reg2)
r2_value2   <- 1 - sum((test_data_clean$SalePrice - pred_knn_reg2)^2) / sum((test_data_clean$SalePrice - mean(test_data_clean$SalePrice))^2)

cat("\nResultados del modelo de regresión KNN con validación cruzada (k =", k_value, "):\n")
cat("RMSE:", round(rmse_value2, 2), "\n")
cat("MAE:", round(mae_value2, 2), "\n")
cat("R-squared:", round(r2_value2, 2), "\n")


# ------------------------------------------------------------------
# Entrenar modelo de regresión KNN con diferentes valores de k
# ------------------------------------------------------------------
params2 <- expand.grid(k = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))
knn_reg3 <- train(SalePrice ~ ., 
                  data = train_complete, 
                  method = "knn", 
                  preProcess = c("center", "scale"), 
                  tuneGrid = params2)
pred_knn_reg3 <- predict(knn_reg3, newdata = test_data_clean)


# Encontrar el mejor valor de k
ggplot(knn_reg3$results, aes(x = k, y = RMSE)) +
  geom_line(color = "orange") +
  geom_point(color = "darkgreen") +
  labs(title = "Valor de k vs. RMSE",
       x = "k", y = "RMSE") +
  theme_minimal()



df_result3 <- data.frame(Index = 1:nrow(test_data_clean),
                        Real = test_data_clean$SalePrice,
                        Pred = pred_knn_reg3)

ggplot(df_result3, aes(x = Index)) +
  geom_point(aes(y = Real, color = "Real"), size = 2) +
  geom_point(aes(y = Pred, color = "Pred"), size = 2) +
  scale_color_manual(values = c("Real" = "blue", "Pred" = "red")) +
  labs(title = "Comparación de valores reales y predichos",
       x = "Índice", y = "Precio de venta", color = "Serie") +
  theme_minimal()

residuos <- test_data_clean$SalePrice - pred_knn_reg3

df_residuos <- data.frame(Real = test_data_clean$SalePrice,
                          Pred = pred_knn_reg3,
                          Residuos = residuos)

ggplot(df_residuos, aes(x = Real, y = Residuos)) +
  geom_point(color = "red", size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Residuos vs. Valores reales",
       x = "Valores reales", y = "Residuos") +
  theme_minimal()

  
# ------------------------------------------------------------------
# Evaluación del modelo
# ------------------------------------------------------------------

rmse_value3 <- rmse(test_data_clean$SalePrice, pred_knn_reg3)
mae_value3  <- mae(test_data_clean$SalePrice, pred_knn_reg3)
r2_value3   <- 1 - sum((test_data_clean$SalePrice - pred_knn_reg3)^2) / sum((test_data_clean$SalePrice - mean(test_data_clean$SalePrice))^2)
mse_value3  <- mean((test_data_clean$SalePrice - pred_knn_reg3)^2)

cat("\nResultados del modelo de regresión KNN con diferentes valores de k:\n")
cat("RMSE:", round(rmse_value3, 2), "\n")
cat("MAE:", round(mae_value3, 2), "\n")
cat("R-squared:", round(r2_value3, 2), "\n")


# ------------------------------------------------------------------ 
# Comparación de modelos de Regresión KNN
# ------------------------------------------------------------------
# ====================================================
# Sección 1: Cargar Librerías y Datos
# ====================================================
library(caret)         # Para modelado, validación y tuning
library(ggplot2)       # Para gráficos
library(dplyr)         # Para manipulación de datos
library(e1071)         # Para el modelo Naive Bayes
library(rpart)         # Para el árbol de regresión
library(rpart.plot)    # Para visualizar el árbol
library(randomForest)  # Para Random Forest
library(Metrics)       # Para RMSE, MAE, etc.
library(reshape2)      # Para reorganizar data frames en formato largo (melt)

# Cargar datos preprocesados
train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)

# Ajustar niveles de variables categóricas en test_data para que coincidan con train_data
factor_vars <- names(train_data)[sapply(train_data, is.factor)]
for (var in factor_vars) {
  if (var %in% names(test_data)) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}

# Eliminar filas con NA para asegurar consistencia en todos los modelos
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]

# Definir predictores (todas excepto la variable respuesta)
predictors <- setdiff(names(train_data), "SalePrice")

# Para modelos que usan los datos originales (Regresión Lineal, Árbol, RF) descartamos la variable discreta si existe.
train_filtered <- train_data[, !(names(train_data) %in% "SalePrice_bin")]
test_filtered  <- test_data[, !(names(test_data) %in% "SalePrice_bin")]

# ====================================================
# Sección 2: Modelo de Naive Bayes para Regresión
# ====================================================
# Objetivo: Discretizar la variable SalePrice, entrenar un modelo Naive Bayes y obtener una predicción "continua"
n_bins <- 50
unique_vals <- length(unique(train_data$SalePrice))
n_bins <- min(n_bins, unique_vals - 1)
# Crear cortes basados en cuantiles
bins <- quantile(train_data$SalePrice, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
bins <- unique(bins)  # Evitar cortes repetidos
# Crear la variable discretizada
train_data$SalePrice_bin <- cut(train_data$SalePrice, breaks = bins, include.lowest = TRUE, dig.lab = 10)
# Calcular el centro de cada bin (valor esperado)
bin_centers <- (head(bins, -1) + tail(bins, -1)) / 2

# Entrenar el modelo Naive Bayes usando los predictores y la variable discreta
nb_model_reg <- naiveBayes(SalePrice_bin ~ ., data = train_data[, c(predictors, "SalePrice_bin")])
# Predecir en test_data: obtener las probabilidades para cada bin
nb_pred_probs <- predict(nb_model_reg, newdata = test_data[, predictors], type = "raw")
# Calcular la predicción final como el valor esperado
nb_pred_reg <- apply(nb_pred_probs, 1, function(prob_vec) sum(prob_vec * bin_centers))
# Métricas para Naive Bayes
rmse_nb <- RMSE(nb_pred_reg, test_data$SalePrice)
mae_nb  <- mae(test_data$SalePrice, nb_pred_reg)
mse_nb  <- mean((test_data$SalePrice - nb_pred_reg)^2)
r2_nb   <- 1 - sum((test_data$SalePrice - nb_pred_reg)^2) / sum((test_data$SalePrice - mean(test_data$SalePrice))^2)

# ====================================================
# Sección 3: Modelos de Regresión Lineal (Stepwise)
# ====================================================
modelo_stepwise <- step(lm(SalePrice ~ ., data = train_filtered),
                        direction = "backward",
                        scope = list(upper = ~ ., lower = ~ 1),
                        trace = FALSE)
pred_lin <- predict(modelo_stepwise, newdata = test_filtered)
rmse_lin <- RMSE(pred_lin, test_filtered$SalePrice)
mae_lin  <- mae(test_filtered$SalePrice, pred_lin)
mse_lin  <- mean((test_filtered$SalePrice - pred_lin)^2)
r2_lin   <- 1 - sum((test_filtered$SalePrice - pred_lin)^2) / sum((test_filtered$SalePrice - mean(test_filtered$SalePrice))^2)




# ====================================================
# Sección 4: Modelo de Árbol de Regresión (Base)
# ====================================================
tree_model <- rpart(SalePrice ~ ., data = train_filtered, method = "anova")
pred_tree <- predict(tree_model, newdata = test_filtered)
rmse_tree <- RMSE(pred_tree, test_filtered$SalePrice)
mae_tree  <- mae(test_filtered$SalePrice, pred_tree)
mse_tree  <- mean((test_filtered$SalePrice - pred_tree)^2)
r2_tree   <- 1 - sum((test_filtered$SalePrice - pred_tree)^2) / sum((test_filtered$SalePrice - mean(test_filtered$SalePrice))^2)

# ====================================================
# Sección 5: Modelo de Random Forest
# ====================================================
set.seed(123)
control_cv_reg <- trainControl(method = "cv", number = 10)
grid_rf <- expand.grid(mtry = c(2, 4, 6, 8))
rf_model <- train(SalePrice ~ ., 
                  data = train_filtered, 
                  method = "rf",
                  trControl = control_cv_reg,
                  tuneGrid = grid_rf,
                  metric = "RMSE")
pred_rf <- predict(rf_model, newdata = test_filtered)
rmse_rf <- RMSE(pred_rf, test_filtered$SalePrice)
mae_rf  <- mae(test_filtered$SalePrice, pred_rf)
mse_rf  <- mean((test_filtered$SalePrice - pred_rf)^2)
r2_rf   <- 1 - sum((test_filtered$SalePrice - pred_rf)^2) / sum((test_filtered$SalePrice - mean(test_filtered$SalePrice))^2)

# ====================================================
# Sección 6: Comparación de Modelos
# ====================================================
df_metrics <- data.frame(
  Model = c("Naive Bayes", "Linear Regression", "Tree Regression", "Random Forest", "KNN"),
  RMSE  = c(rmse_nb, rmse_lin, rmse_tree, rmse_rf, rmse_value3),
  MAE   = c(mae_nb, mae_lin, mae_tree, mae_rf, mae_value3),
  MSE   = c(mse_nb, mse_lin, mse_tree, mse_rf, mse_value3),
  R2    = c(r2_nb, r2_lin, r2_tree, r2_rf, r2_value3)
)


cat("Comparación Final de Métricas:\n")
print(df_metrics)

# ====================================================
# Sección 7: Gráficos Comparativos
# ====================================================
# Crear un data frame con las predicciones de cada modelo y los valores reales
common_idx <- 1:length(nb_pred_reg)
df_pred <- data.frame(
  Actual = test_filtered$SalePrice[common_idx],
  NaiveBayes = nb_pred_reg,
  Linear = pred_lin[common_idx],
  Tree = pred_tree[common_idx],
  RF = pred_rf[common_idx],
  KNN = pred_knn_reg1[common_idx]
)
# Reorganizar en formato largo
df_pred_melt <- melt(df_pred, id.vars = "Actual", variable.name = "Model", value.name = "Predicted")

# Gráfico de Predicción vs. Valor Real por modelo (facetas)
ggplot(df_pred_melt, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~Model, scales = "free") +
  labs(title = "Predicción vs. Valor Real por Modelo",
       x = "Valor Real (SalePrice)",
       y = "Valor Predicho") +
  theme_minimal()

# ------------------------------------------------------------------
# Sección 9: Prueba de HIPERPARÁMETROS PARA TODOS LOS MODELOS DE REGRESIÓN
# ------------------------------------------------------------------
library(kknn)

# ====================================================
# 1. Tuning Avanzado KNN con "kknn"
# ====================================================

# Definir una grilla de hiperparámetros para kknn:
# - kmax: número máximo de vecinos a considerar (en este ejemplo se prueban 5 y 9 y 13)
# - distance: tipo de distancia; 1 para Manhattan y 2 para Euclidiana
# - kernel: método de ponderación de los vecinos ("rectangular" para sin ponderación, "triangular" para dar mayor peso a vecinos más cercanos)
grid_knn_tuning <- expand.grid(
  kmax = seq(5, 13, by = 4),      # Probamos kmax = 5, 9, 13
  distance = c(1, 2),             # Distancia Manhattan y Euclidiana
  kernel = c("rectangular", "triangular")  # Dos esquemas de ponderación
)

# Configurar validación cruzada: se usa 10-fold CV
tr_control <- trainControl(method = "cv", number = 10)

# Ajustar el modelo KNN de regresión con la grilla definida
set.seed(123)
knn_tuned_model <- train(
  SalePrice ~ .,                  # Fórmula: predecir SalePrice usando todas las demás variables
  data = train_complete,          # Conjunto de entrenamiento completo
  method = "kknn",                # Método kknn, que permite tunear múltiples hiperparámetros
  preProcess = c("center", "scale"),  # Normalización, muy importante para KNN
  trControl = tr_control,         # Control de validación cruzada
  tuneGrid = grid_knn_tuning,     # La grilla de hiperparámetros definida
  metric = "RMSE"                 # Métrica a optimizar
)

# Mostrar la mejor combinación de hiperparámetros encontrada
cat("Mejor combinación de hiperparámetros (kmax, distance, kernel):\n")
print(knn_tuned_model$bestTune)

# Realizar predicciones en el conjunto de prueba
pred_knn_tuned <- predict(knn_tuned_model, newdata = test_data_clean)

# Calcular las métricas de evaluación
rmse_knn_tuned <- rmse(test_data_clean$SalePrice, pred_knn_tuned)
mae_knn_tuned  <- mae(test_data_clean$SalePrice, pred_knn_tuned)
mse_knn_tuned  <- mean((test_data_clean$SalePrice - pred_knn_tuned)^2)
r2_knn_tuned   <- 1 - sum((test_data_clean$SalePrice - pred_knn_tuned)^2) / 
  sum((test_data_clean$SalePrice - mean(test_data_clean$SalePrice))^2)

# Imprimir las métricas obtenidas
cat("\nResultados del modelo KNN tunado:\n")
cat("RMSE:", round(rmse_knn_tuned, 2), "\n")
cat("MAE:", round(mae_knn_tuned, 2), "\n")
cat("MSE:", round(mse_knn_tuned, 2), "\n")
cat("R-squared:", round(r2_knn_tuned, 2), "\n")


# Crear un data frame con el índice, el valor real y el valor predicho
df_result_tuned <- data.frame(
  Index = 1:nrow(test_data_clean),
  Real = test_data_clean$SalePrice,
  Pred = pred_knn_tuned
)

# Graficar la comparación de los valores reales y predichos
ggplot(df_result_tuned, aes(x = Index)) +
  geom_point(aes(y = Real, color = "Real"), size = 2) +
  geom_point(aes(y = Pred, color = "Pred"), size = 2) +
  scale_color_manual(values = c("Real" = "blue", "Pred" = "red")) +
  labs(title = "Comparación de valores reales y predichos (Modelo KNN Tunado)",
       x = "Índice", y = "Precio de venta", color = "Serie") +
  theme_minimal()


# ====================================================
# 2. Tuning Random Forest
# ====================================================
grid_rf <- expand.grid(mtry = c(2, 4, 6, 8))

set.seed(123)
rf_model_tuned <- train(
  SalePrice ~ .,
  data = train_filtered,
  method = "rf",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = grid_rf,
  metric = "RMSE"
)

pred_rf_tuned <- predict(rf_model_tuned, newdata = test_filtered)
rmse_rf_tuned <- RMSE(pred_rf_tuned, test_filtered$SalePrice)
mae_rf_tuned  <- mae(test_filtered$SalePrice, pred_rf_tuned)
mse_rf_tuned <- mean((test_filtered$SalePrice - pred_rf_tuned)^2)
r2_rf_tuned   <- 1 - sum((test_filtered$SalePrice - pred_rf_tuned)^2) / sum((test_filtered$SalePrice - mean(test_filtered$SalePrice))^2)

# ====================================================
# 3. Tuning Árbol de Regresión
# ====================================================
grid_tree <- expand.grid(cp = seq(0.001, 0.05, length.out = 6))

tree_model_tuned <- train(
  SalePrice ~ .,
  data = train_filtered,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = grid_tree
)

pred_tree_tuned <- predict(tree_model_tuned, newdata = test_filtered)
rmse_tree_tuned <- RMSE(pred_tree_tuned, test_filtered$SalePrice)
mae_tree_tuned  <- mae(test_filtered$SalePrice, pred_tree_tuned)
mse_tree_tuned <- mean((test_filtered$SalePrice - pred_tree_tuned)^2)
r2_tree_tuned   <- 1 - sum((test_filtered$SalePrice - pred_tree_tuned)^2) / sum((test_filtered$SalePrice - mean(test_filtered$SalePrice))^2)

# ====================================================
# 4. Tuning Naive Bayes (variando número de bins)
# ====================================================
bin_options <- c(25, 50, 75, 100)
results_nb <- data.frame()

for (b in bin_options) {
  bins <- quantile(train_data$SalePrice, probs = seq(0, 1, length.out = b + 1), na.rm = TRUE)
  bins <- unique(bins)
  if (length(bins) < 3) next
  
  train_data$SalePrice_bin <- cut(train_data$SalePrice, breaks = bins, include.lowest = TRUE, dig.lab = 10)
  bin_centers <- (head(bins, -1) + tail(bins, -1)) / 2
  
  nb_model <- naiveBayes(SalePrice_bin ~ ., data = train_data[, c(predictors, "SalePrice_bin")])
  nb_probs <- predict(nb_model, newdata = test_data[, predictors], type = "raw")
  nb_pred <- apply(nb_probs, 1, function(prob_vec) sum(prob_vec * bin_centers))
  
  rmse <- RMSE(nb_pred, test_data$SalePrice)
  mae  <- mae(test_data$SalePrice, nb_pred)
  mse  <- mean((test_data$SalePrice - nb_pred)^2)
  r2   <- 1 - sum((test_data$SalePrice - nb_pred)^2) / sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
  
  results_nb <- rbind(results_nb, data.frame(Bins = b, RMSE = rmse, MAE = mae, MSE = mse, R2 = r2))
}

best_nb_row <- results_nb[which.min(results_nb$RMSE), ]

mse_nb_tuned <- best_nb_row$MSE

# ====================================================
# Comparación Final de TODOS los modelos Tuned
# ====================================================


df_metrics_tuned <- data.frame(
  Model = c("KNN Tuned", "Random Forest Tuned", "Tree Tuned", "Naive Bayes Tuned"),
  RMSE  = c(rmse_knn_tuned, rmse_rf_tuned, rmse_tree_tuned, best_nb_row$RMSE),
  MAE   = c(mae_knn_tuned, mae_rf_tuned, mae_tree_tuned, best_nb_row$MAE),
  MSE   = c(mse_knn_tuned, mse_rf_tuned, mse_tree_tuned, mse_nb_tuned),
  R2    = c(r2_knn_tuned, r2_rf_tuned, r2_tree_tuned, best_nb_row$R2)
)


cat("\nComparación de Modelos Ajustados (Tuned):\n")
print(df_metrics_tuned)




  



