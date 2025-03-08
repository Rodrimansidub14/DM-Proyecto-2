###############################################
# Script: Árboles de Decisión y Random Forest
# Proyecto: Predicción y Clasificación del Precio de Casas
# CC3074 – Minería de Datos, Semestre I – 2025
# Universidad del Valle de Guatemala
###############################################

# ----------------------------
# Paso 1. Cargar conjuntos de entrenamiento y prueba
# ----------------------------


# Cargar librerías necesarias
library(GGally)
library(nortest)
library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(discretization)
# Cargar datos preprocesados (los mismos usados para regresión lineal)
train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)

cat("Dimensiones originales de train:", dim(train_data), "\n")
cat("Dimensiones originales de test:", dim(test_data), "\n")

# Para asegurar consistencia: ajustar niveles de variables categóricas problemáticas
test_data$Condition2 <- factor(test_data$Condition2, levels = levels(train_data$Condition2))
test_data$RoofMatl   <- factor(test_data$RoofMatl,   levels = levels(train_data$RoofMatl))

# ----------------------------
# Paso 1.1. Filtrar únicamente filas completas (sin NA)
# ----------------------------
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]
cat("Dimensiones de train (sin NA):", dim(train_data), "\n")
cat("Dimensiones de test (sin NA):", dim(test_data), "\n")

# ----------------------------
# Paso 2. Elaborar un árbol de regresión (modelo base) para predecir SalePrice
# ----------------------------
# Se utiliza el método "anova" para regresión
modelo_reg_base <- rpart(SalePrice ~ ., data = train_data, method = "anova")
rpart.plot(modelo_reg_base, main = "Arbol de Regresion: Modelo Base")

# ----------------------------
# Paso 3. Preprocesamiento extra: ajustar niveles y escalar variables
# ----------------------------
# Ajustar niveles de variables categóricas en el conjunto de prueba para que coincidan con el de entrenamiento
factor_vars <- names(train_data)[sapply(train_data, is.factor)]
for (var in factor_vars) {
  if (var %in% names(test_data)) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}



# ----------------------------
# Paso 3.1. Predicción y evaluación del modelo base de regresión
# ----------------------------
pred_reg_test <- predict(modelo_reg_base, newdata = test_data)
rmse_reg_test <- RMSE(pred_reg_test,test_data$SalePrice)
mse_reg_test  <- mean((pred_reg_test - test_data$SalePrice)^2)
cat("RMSE del Árbol de Regresión (Modelo Base):", rmse_reg_test, "\n")
cat("MSE del Árbol de Regresión (Modelo Base):", mse_reg_test, "\n")

pred_reg_train <- predict(modelo_reg_base, newdata = train_data)
rmse_reg_train <- RMSE(pred_reg_train, train_data$SalePrice)
mse_reg_train  <- mean((pred_reg_train - train_data$SalePrice)^2)
cat("RMSE en entrenamiento (Modelo Base):", rmse_reg_train, "\n")
cat("MSE en entrenamiento (Modelo Base):", mse_reg_train, "\n")

# Gráfico de predicción
plot(test_data$SalePrice, col = "blue", main = "Prediccion vs Real (Modelo Base)",
     xlab = "Indice", ylab = "SalePrice")
points(pred_reg_test, col = "red")
legend("topright", legend = c("Real", "Prediccion"), col = c("blue", "red"), pch = 1)

# ----------------------------
# Paso 4. Elaborar al menos 3 modelos de regresión modificando el parámetro maxdepth
# ----------------------------
# Se definen modelos con validación cruzada
# (Reiniciamos el set de datos filtrados si fuera necesario)
train_data <- na.omit(train_data)

# Definir control de validación cruzada
control_cv <- trainControl(method = "cv", number = 15)

# Entrenar modelo con caret usando rpart
modelo_reg_cv <- caret::train(SalePrice ~ ., 
                              data = train_data, 
                              method = "rpart", 
                              trControl = control_cv)
print(modelo_reg_cv)
rpart.plot(modelo_reg_cv$finalModel, main = "Arbol de Regresion Tuned con CV")

# Predicciones y evaluación para el modelo tunado por CV
pred_reg_cv_test <- predict(modelo_reg_cv, newdata = test_data)
rmse_reg_cv_test <- RMSE(pred_reg_cv_test, test_data$SalePrice)
cat("RMSE del Arbol de Regresion (Modelo CV):", rmse_reg_cv_test, "\n")
pred_reg_cv_train <- predict(modelo_reg_cv, newdata = train_data)
rmse_reg_cv_train <- RMSE(pred_reg_cv_train, train_data$SalePrice)
cat("RMSE en entrenamiento (Modelo CV):", rmse_reg_cv_train, "\n")

plot(test_data$SalePrice, col = "blue", main = "Prediccion vs Real (Modelo CV)",
     xlab = "Indice", ylab = "SalePrice")
points(pred_reg_cv_test, col = "red")
legend("topright", legend = c("Real", "Prediccion"), col = c("blue", "red"), pch = 1)


##parametrons a tunear
modelLookup("rpart")
modelLookup("rpart2")


# Modelos manuales con diferentes maxdepth
# Modelo con maxdepth = 3
# Define el grid de profundidad
depths <- data.frame(maxdepth = c(1,3, 5, 7))
modelo_reg_depth3 <- caret::train(SalePrice ~ ., 
                                  data = train_data, 
                                  method = "rpart2", 
                                  trControl = control_cv,
                                  metric = "RMSE", 
                                  tuneGrid = depths)

plot(modelo_reg_depth3)

#modelo greedy

modelo_reg3 <- predict(modelo_reg_depth3, newdata = test_data)
rmse_reg3 <- RMSE(modelo_reg3, test_data$SalePrice)
cat("RMSE del Arbol de Regresion (Modelo maxdepth = 3):", rmse_reg3, "\n")
mse_reg3  <- mean((modelo_reg3 - test_data$SalePrice)^2)
cat("MSE del Arbol de Regresion (Modelo maxdepth = 3):", mse_reg3, "\n")


pred_reg3_train <- predict(modelo_reg_depth3, newdata = train_data)
rmse_reg3_train <- RMSE(pred_reg3_train, train_data$SalePrice)
cat("RMSE en entrenamiento (Modelo maxdepth = 3):", rmse_reg3_train, "\n")
mse_reg3_train  <- mean((pred_reg3_train - train_data$SalePrice)^2)
cat("MSE en entrenamiento (Modelo maxdepth = 3):", mse_reg3_train, "\n")

plot(test_data$SalePrice, col = "blue", main = "Prediccion vs Real (Modelo maxdepth = 3)",
     xlab = "Indice", ylab = "SalePrice")
points(modelo_reg3, col = "red")
legend("topright", legend = c("Real", "Prediccion"), col = c("blue", "red"), pch = 1)

comparemodels <- c(rmse_reg_test, rmse_reg_cv_test, rmse_reg3)
names(comparemodels) <- c("Modelo Base", "Modelo CV", "Modelo maxdepth = 3")
print(comparemodels)


# -----------------------------------------------------------------
# PASO 6. CREACIÓN DE LA VARIABLE RESPUESTA PARA CLASIFICACIÓN
# (Económicas, Intermedias, Caras) CON BASE EN LA DISTRIBUCIÓN DE PRECIOS
# -----------------------------------------------------------------

# Análisis rápido de la distribución de precios en el conjunto de entrenamiento
cat("\nResumen de SalePrice en train:\n")
print(summary(train_data$SalePrice))

# Tomar cuartiles como referencia (ejemplo)
cuartiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75))
lower_threshold <- cuartiles[1]  # ~ primer cuartil
upper_threshold <- cuartiles[2]  # ~ tercer cuartil

cat("\nUmbrales elegidos para clasificar:\n")
cat("Económicas: < ", lower_threshold, "\n")
cat("Intermedias: [", lower_threshold, ", ", upper_threshold, ")\n")
cat("Caras: >= ", upper_threshold, "\n\n")

# Crear la nueva variable categórica en train_data
train_data$PriceCat <- case_when(
  train_data$SalePrice < lower_threshold ~ "Economicas",
  train_data$SalePrice < upper_threshold ~ "Intermedias",
  TRUE ~ "Caras"
)
train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas","Intermedias","Caras"))

# Crear la misma variable en test_data
test_data$PriceCat <- case_when(
  test_data$SalePrice < lower_threshold ~ "Economicas",
  test_data$SalePrice < upper_threshold ~ "Intermedias",
  TRUE ~ "Caras"
)
test_data$PriceCat <- factor(test_data$PriceCat, levels = c("Economicas","Intermedias","Caras"))

cat("Distribución de PriceCat en train:\n")
print(table(train_data$PriceCat))
cat("\nDistribución de PriceCat en test:\n")
print(table(test_data$PriceCat))

# ----------------------------
# PASO 7. Elaborar un árbol de clasificación con la nueva variable PriceCat
# ----------------------------
# Importante: no incluimos SalePrice como predictor, pues PriceCat se derivó de él
formula_class <- PriceCat ~ . - SalePrice

# Entrenar el árbol de clasificación "base"
modelo_class_base <- rpart(formula_class, data = train_data, method = "class")
rpart.plot(modelo_class_base, main = "Arbol de Clasificacion Base (PriceCat)")

pred_class_base <- predict(modelo_class_base, newdata = test_data, type = "class")




conf_matrix_base <- confusionMatrix(pred_class_base, test_data$PriceCat)
cat("Matriz de Confusión - Árbol de Clasificación (Modelo Base):\n")
print(conf_matrix_base)

depths <- c(2, 4, 6)
resultados_depth <- data.frame(Profundidad = depths, Accuracy = NA)

# ----------------------------
# Paso 9. Análisis de la eficiencia del árbol de clasificación (matriz de confusión)
# ----------------------------
print(conf_matrix_base$table)
# En el informe se debe detallar cuál clase se predice mejor y dónde se cometen más errores

# ----------------------------
# Paso 10. Entrenar un modelo de clasificación usando validación cruzada y predecir con él
# ----------------------------
set.seed(123)
control_cv_class <- trainControl(method = "cv", number = 10)
grid_class <- expand.grid(maxdepth = c(2, 3, 4, 5, 6))
modelo_class_cv <- train(formula_class,
                         data = train_data,
                         method = "rpart2",
                         tuneGrid = grid_class,
                         trControl = control_cv_class,
                         metric = "Accuracy")
print(modelo_class_cv)
rpart.plot(modelo_class_cv$finalModel, main = "Árbol de Clasificación Tuned con CV")
pred_class_cv <- predict(modelo_class_cv, newdata = test_data)
conf_matrix_cv <- confusionMatrix(pred_class_cv, test_data$PriceCat)
cat("Matriz de Confusión - Árbol de Clasificación (Modelo CV):\n")
print(conf_matrix_cv)

# ----------------------------
# Paso 11. Elaborar al menos 3 modelos más de clasificación cambiando la profundidad del árbol
# ----------------------------
depths <- c(2, 4, 6)
resultados_depth <- data.frame(Profundidad = depths, Accuracy = NA)
for (i in seq_along(depths)) {
  modelo_temp <- rpart(formula_class, data = train_data, method = "class",
                       control = rpart.control(maxdepth = depths[i]))
  pred_temp <- predict(modelo_temp, newdata = test_data, type = "class")
  cm_temp <- confusionMatrix(pred_temp, test_data$PriceCat)
  resultados_depth$Accuracy[i] <- cm_temp$overall["Accuracy"]
  cat("Modelo con maxdepth =", depths[i], "tiene Accuracy =", round(cm_temp$overall["Accuracy"],4), "\n")
}
print(resultados_depth)

# ----------------------------
# Paso 12. Repetir análisis usando Random Forest para clasificación y comparar algoritmos
# ----------------------------
set.seed(123)
modelo_rf_class <- randomForest(PriceCat ~ . - SalePrice, data = train_data, na.action = na.omit)
pred_rf_class <- predict(modelo_rf_class, newdata = test_data)
conf_matrix_rf <- confusionMatrix(pred_rf_class, test_data$PriceCat)
cat("Matriz de Confusión - Random Forest para Clasificación:\n")
print(conf_matrix_rf)

###############################################
# FIN DEL SCRIPT
###############################################
