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
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)

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

# Escalar variables numéricas usando media y desviación estándar del conjunto de entrenamiento
num_vars <- names(train_data)[sapply(train_data, is.numeric)]
for (var in num_vars) {
  media   <- mean(train_data[[var]], na.rm = TRUE)
  sd_val  <- sd(train_data[[var]], na.rm = TRUE)
  train_data[[var]] <- (train_data[[var]] - media) / sd_val
  test_data[[var]]  <- (test_data[[var]] - media) / sd_val
}

# ----------------------------
# Paso 3.1. Predicción y evaluación del modelo base de regresión
# ----------------------------
pred_reg_test <- predict(modelo_reg_base, newdata = test_data)
rmse_reg_test <- RMSE(pred_reg_test, test_data$SalePrice)
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
control_cv <- trainControl(method = "cv", number = 10)

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
depths <-data.frame(c(2, 4, 6,8,10))
names(depths) <- "maxdepth"
system.time(
modelo_reg_depth3 <- 

------------------------
# Paso 5. Comparar resultados del árbol de regresión con el modelo de regresión lineal previo
# ----------------------------
modelo_lineal <- lm(SalePrice ~ ., data = train_data)
pred_lineal <- predict(modelo_lineal, newdata = test_data)
rmse_lineal <- RMSE(pred_lineal, test_data$SalePrice)
cat("RMSE del Modelo de Regresión Lineal:", rmse_lineal, "\n")
# Se debe comparar y comentar cuál modelo predice mejor SalePrice

# ----------------------------
# Paso 6. Crear variable respuesta para clasificar las casas en Económicas, Intermedias o Caras
# ----------------------------
# Utilizamos los cuantiles de SalePrice del conjunto de entrenamiento para definir los límites
q <- quantile(train_data$SalePrice, probs = c(0, 1/3, 2/3, 1))
train_data$PriceCat <- cut(train_data$SalePrice, breaks = q,
                           labels = c("Económicas", "Intermedias", "Caras"),
                           include.lowest = TRUE)
test_data$PriceCat <- cut(test_data$SalePrice, breaks = q,
                          labels = c("Económicas", "Intermedias", "Caras"),
                          include.lowest = TRUE)

cat("Distribución de PriceCat en entrenamiento:\n")
print(table(train_data$PriceCat))

# ----------------------------
# Paso 7. Elaborar un árbol de clasificación utilizando la variable respuesta creada
# ----------------------------
# Excluir SalePrice para evitar contaminación en el entrenamiento
predictors_class <- setdiff(names(train_data), c("SalePrice", "PriceCat"))
formula_class <- as.formula(paste("PriceCat ~", paste(predictors_class, collapse = " + ")))
modelo_class_base <- rpart(formula_class, data = train_data, method = "class")
rpart.plot(modelo_class_base, main = "Árbol de Clasificación: Modelo Base")
# Comenta en el informe las variables clave y la interpretación de los nodos

# ----------------------------
# Paso 8. Utilizar el modelo de clasificación con el conjunto de prueba y determinar su eficiencia
# ----------------------------
pred_class_base <- predict(modelo_class_base, newdata = test_data, type = "class")
conf_matrix_base <- confusionMatrix(pred_class_base, test_data$PriceCat)
cat("Matriz de Confusión - Árbol de Clasificación (Modelo Base):\n")
print(conf_matrix_base)

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
