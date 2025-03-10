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
library(randomForest)

library(rpart.plot)
library(caret)
library(discretization)
library(mlr)
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

accuracy <- conf_matrix_base$overall["Accuracy"]
cat("Accuracy global:", accuracy, "\n")

# - Métricas por clase (sensibilidad, especificidad, etc.)
cat("Métricas por clase:\n")
print(conf_matrix_base$byClass)



# ----------------------------
# Paso 09. Entrenar un modelo de clasificación usando validación cruzada y predecir con él
# ----------------------------
depths <- expand.grid(maxdepth = c(2,4,6,8,10))

system.time(modelo_class_cv <- caret::train(formula_class, 
                                            data = train_data, 
                                            method = "rpart2",      
                                            tuneGrid = expand.grid(maxdepth = 2:10),
                                            trControl = control_cv,
                                            metric = "Accuracy")
)



modelo_class_cv
rpart.plot(modelo_class_cv$finalModel, main = "Arbol de Clasificacion Tuned con CV (PriceCat)")
                                     


# ----------------------------
# Paso 11. Elaborar al menos 3 modelos más de clasificación cambiando la profundidad del árbol
# ----------------------------
depths <- c(3, 5, 7)
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
# Paso 12. Repetir análisis usando Random Forest 

library(randomForest)
library(caret)

set.seed(123)
# Definir control de validación cruzada para regresión (por ejemplo, 10-fold CV)
control_cv_reg <- trainControl(method = "cv", number = 10)

# Definir un grid para tunear el parámetro mtry (número de variables a considerar en cada división)
grid_rf <- expand.grid(mtry = c(2, 4, 6, 8))

# Entrenar el modelo de Random Forest para predecir SalePrice usando caret
modelo_rf_reg <- caret::train(SalePrice ~ ., 
                              data = train_data, 
                              method = "rf",
                              trControl = control_cv_reg,
                              tuneGrid = grid_rf,
                              metric = "RMSE")
  
# Mostrar resultados del tuning
print(modelo_rf_reg)
plot(modelo_rf_reg, main = "Tuning de Random Forest para Regresión")

# Realizar predicciones en el conjunto de prueba
ypred_rf_reg <- predict(modelo_rf_reg, newdata = test_data)

# Calcular y mostrar el RMSE en el conjunto de prueba
rmse_rf_reg <- RMSE(ypred_rf_reg, test_data$SalePrice)
cat("RMSE del Random Forest tuneado para regresión:", rmse_rf_reg, "\n")

# Opcional: Graficar las predicciones vs valores reales
# Extraer los índices de las filas que generaron predicción
idx <- as.numeric(names(ypred_rf_reg))
actual <- test_data$SalePrice[idx]

# Graficar las predicciones vs. los valores reales correspondientes
plot(actual, ypred_rf_reg, 
     main = "Predicción vs. Real (Random Forest Regresión)",
     xlab = "Valores Reales de SalePrice", 
     ylab = "Predicciones",
     col = "blue", pch = 16)
abline(a = 0, b = 1, col = "red", lwd = 2)


# Suponiendo que 'ypred_rf_reg' contiene las predicciones del modelo para el conjunto de prueba:
plot(test_data$SalePrice, col = "blue", main = "Predicción vs Real (Modelo maxdepth = 3)",
     xlab = "Índice", ylab = "SalePrice")
points(ypred_rf_reg, col = "red", pch = 1)
legend("topright", legend = c("Real", "Predicción"), col = c("blue", "red"), pch = 1)


# Paso 12. Repetir análisis usando Random Forest para clasificación y comparar algoritmos

# Extraer la variable de referencia del conjunto de prueba
y <- test_data[,"PriceCat"]

# Entrenar modelo de Random Forest para clasificación 
# (se excluye SalePrice, ya que PriceCat se deriva de él)
modelo_rf <- randomForest(PriceCat ~ . - SalePrice, data = train_data, na.action = na.omit)

# Realizar predicciones en el conjunto de prueba
ypred <- predict(modelo_rf, newdata = test_data)

# Asegurarse de que las predicciones sean factores
ypred <- factor(ypred, levels = levels(train_data$PriceCat))

  # Calcular la matriz de confusión
confusionMatrix(ypred, y)


# ----------------------------
# Ajuste de hiperparámetros usando mlr (para árboles de decisión) con PriceCat como target
# ----------------------------

# Se consulta el set de parámetros disponibles para classif.rpart
getParamSet("classif.rpart")

# Crear la tarea de clasificación con mlr usando PriceCat como variable objetivo
classifier <- makeClassifTask(data = train_data, target = "PriceCat")

# Definir la tabla de parámetros a tunear (por ejemplo, maxdepth de 1 a 16)
paramTable <- makeParamSet(makeDiscreteParam("maxdepth", values = 1:16))

# Control para tuning: grid search
controlGrid <- makeTuneControlGrid()

# Descripción del remuestreo: validación cruzada con 4 iteraciones
cv <- makeResampleDesc("CV", iters = 4L)

# Definir la métrica: accuracy
metric <- acc

# Ajuste de hiperparámetros con tuneParams 
dtune <- tuneParams(
  learner = "classif.rpart",
  task = classifier,
  resampling = cv,
  measures = metric,
  par.set = paramTable,
  control = controlGrid,
  show.info = TRUE
)

# Generar el efecto de los hiperparámetros (corregido el nombre de la función)
result_Hyper <- generateHyperParsEffectData(dtune, partial.dep = TRUE)

library(ggplot2)
ggplot(
  data = result_Hyper$data,
  aes(x = maxdepth, y = acc.test.mean)
) + 
  geom_line(color = "blue") +
  labs(title = "Efecto de maxdepth en Accuracy (CV)",
       x = "maxdepth",
       y = "Accuracy promedio en test")

# Establecer el mejor hiperparámetro encontrado
best_params <- setHyperPars(
  makeLearner("classif.rpart"),
  par.vals = dtune$x
)

# Entrenar el mejor modelo usando mlr con los mejores parámetros
best_model <- train(best_params, classifier)

# Preparar el conjunto de prueba para mlr (usar PriceCat como target)
d.tree.mlr.test <- makeClassifTask(data = test_data, target = "PriceCat")

# Realizar predicciones con el mejor modelo
res <- predict(best_model, newdata = test_data)$data

# Calcular la matriz de confusión usando caret
confusionMatrix(res$truth, res$response)
