# -----------------------------
# Importación de Librerías
# -----------------------------
library(tidyverse)   # Incluye dplyr y readr para manipulación y lectura de datos
library(ggplot2)
library(fastDummies)
library(profvis)
library(caret)
library(mlr)

# Fijar semilla para la reproducibilidad
set.seed(123)

# -----------------------------
# Lectura de Archivos de Datos
# -----------------------------
# Ajuste las rutas de los archivos según su ubicación
train_data <- read_csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv")
test_data  <- read_csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/test_preprocessed.csv")

# --------------------------------------------------
# Preparación de Datos: Creación de la variable categórica PriceCat
# y de las variables dicotómicas para cada categoría
# --------------------------------------------------
# Definir umbrales basados en los cuartiles de SalePrice en el conjunto de entrenamiento
cuartiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75))
lower_threshold <- cuartiles[1]
upper_threshold <- cuartiles[2]

# En el conjunto de entrenamiento se crean:
# - PriceCat: variable categórica con niveles "Economicas", "Intermedias" y "Caras"
# - Es_Economica: 1 si la vivienda es económica, 0 en otro caso
# - Es_Intermedia: 1 si la vivienda es intermedia, 0 en otro caso
# - Es_Cara: 1 si la vivienda es cara, 0 en otro caso

train_data <- train_data %>%
  mutate(PriceCat = case_when(
    SalePrice < lower_threshold ~ "Economicas",
    SalePrice < upper_threshold ~ "Intermedias",
    TRUE ~ "Caras"
  )) %>%
  mutate(PriceCat = factor(PriceCat, levels = c("Economicas", "Intermedias", "Caras")),
         Es_Economica  = if_else(PriceCat == "Economicas", 1, 0),
         Es_Intermedia = if_else(PriceCat == "Intermedias", 1, 0),
         Es_Cara       = if_else(PriceCat == "Caras",       1, 0)
  )

# Generar la misma variable en el conjunto de prueba (para consistencia en experimentos futuros)
test_data <- test_data %>%
  mutate(PriceCat = case_when(
    SalePrice < lower_threshold ~ "Economicas",
    SalePrice < upper_threshold ~ "Intermedias",
    TRUE ~ "Caras"
  )) %>%
  mutate(PriceCat = factor(PriceCat, levels = c("Economicas", "Intermedias", "Caras")),
         Es_Economica  = if_else(PriceCat == "Economicas", 1, 0),
         Es_Intermedia = if_else(PriceCat == "Intermedias", 1, 0),
         Es_Cara       = if_else(PriceCat == "Caras",       1, 0)
  )

# Mostrar las distribuciones de PriceCat y de las variables dicotómicas en el conjunto de entrenamiento
cat("Distribución de PriceCat en train_data:\n")
print(table(train_data$PriceCat))
cat("\nDistribución de Es_Economica en train_data:\n")
print(table(train_data$Es_Economica))
cat("\nDistribución de Es_Intermedia en train_data:\n")
print(table(train_data$Es_Intermedia))
cat("\nDistribución de Es_Cara en train_data:\n")
print(table(train_data$Es_Cara))

# --------------------------------------------------
# Selección de Variables Predictoras
# --------------------------------------------------
# Para este ejemplo se eligen las variables: OverallQual, GrLivArea y YearBuilt.
# Asegúrese que estas variables existan en train_data; de no estarlo, reemplace por aquellas disponibles y relevantes.
if (!all(c("OverallQual", "GrLivArea", "YearBuilt") %in% names(train_data))) {
  stop("Una o más de las variables predictoras 'OverallQual', 'GrLivArea' o 'YearBuilt' no se encuentran en train_data.")
}

# Definir la fórmula del modelo para predecir Es_Cara (puede cambiarse según la variable dicotómica de interés)
# Convertir la variable respuesta a factor en ambos conjuntos
train_data <- train_data %>% mutate(Es_Cara = factor(Es_Cara, levels = c(0, 1)))
test_data  <- test_data %>% mutate(Es_Cara = factor(Es_Cara, levels = c(0, 1)))

# Ahora definir la fórmula y entrenar el modelo nuevamente
model_formula <- Es_Cara ~ OverallQual + GrLivArea + YearBuilt

cv_control <- trainControl(method = "cv", number = 10)

set.seed(123)
logistic_model <- caret::train(
  model_formula,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = cv_control
)

# Mostrar resumen del modelo final
summary_logistic <- summary(logistic_model$finalModel)
print(summary_logistic)

# Ahora ya se podrá predecir correctamente las probabilidades
pred_probs <- predict(logistic_model, newdata = train_data, type = "prob")


# Convertir a clases: Si la probabilidad predicha de Es_Cara es mayor a 0.5, se clasifica como 1
pred_class <- if_else(pred_probs[, 2] > 0.5, 1, 0)

# Generar y mostrar la matriz de confusión (evaluación en entrenamiento)
conf_matrix <- table(Predicted = pred_class, Actual = train_data$Es_Cara)
print(conf_matrix)

# Calcular y mostrar la precisión
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Precisión del modelo en el conjunto de entrenamiento:", accuracy, "\n")

library(car)

vif_values <- vif(logistic_model$finalModel)
print(vif_values)


predictors <- train_data %>% select(OverallQual, GrLivArea, YearBuilt)
cor_matrix <- cor(predictors)
print(cor_matrix)
corrplot::corrplot(cor_matrix, method = "number")



# --------------------------------------------------
# Predicción en el conjunto de prueba
# --------------------------------------------------

# Convertir Es_Cara a factor en test_data (ya se realizó previamente)
# Generar las probabilidades con el modelo en el conjunto de prueba:
test_pred_probs <- predict(logistic_model, newdata = test_data, type = "prob")

# Convertir a clases usando umbral 0.5:
test_pred_class <- if_else(test_pred_probs[, 2] > 0.5, 1, 0)

# Construir la matriz de confusión:
conf_matrix_test <- table(Predicted = test_pred_class, Actual = test_data$Es_Cara)
print(conf_matrix_test)

# Para métricas adicionales se puede usar:
library(caret)
confusionMatrix(as.factor(test_pred_class), test_data$Es_Cara)


# --------------------------------------------------
# Generación de las Curvas de Aprendizaje para Evaluar Sobreajuste
# --------------------------------------------------
# --------------------------------------------------
# Paso A: Seleccionar únicamente las variables necesarias
# --------------------------------------------------
model_vars <- c("OverallQual", "GrLivArea", "YearBuilt", "Es_Cara")
train_data_filtered <- train_data 
train_model <- train_data_filtered[, model_vars]

# Verificar que la variable target sea factor y tenga al menos dos niveles:
train_model$Es_Cara <- as.factor(train_model$Es_Cara)
if(nlevels(train_model$Es_Cara) < 2) {
  stop("La variable target 'Es_Cara' no tiene dos niveles en el conjunto de entrenamiento.")
}

# --------------------------------------------------
# Paso B: Crear la tarea de clasificación con el subconjunto
# --------------------------------------------------
library(mlr)
task_lc <- makeClassifTask(data = as.data.frame(train_model), target = "Es_Cara")

# --------------------------------------------------
# Paso C: Imputar valores faltantes en la tarea (por si los hubiera)
# --------------------------------------------------
imputed <- impute(task_lc,
                  classes = list(
                    numeric = imputeMedian(),
                    factor  = imputeMode()
                  ))
task_lc <- imputed$task

# --------------------------------------------------
# Paso D: Definir la estrategia de resampling y el aprendiz
# --------------------------------------------------
rin_lc <- makeResampleDesc("CV", iters = 10, predict = "both")
lrn_log_mlr <- makeLearner("classif.logreg", predict.type = "prob")

# --------------------------------------------------
# Paso E: Generar la curva de aprendizaje
# --------------------------------------------------
lc_data <- generateLearningCurveData(
  learners = lrn_log_mlr,
  task = task_lc,
  percs = seq(0.1, 1, by = 0.1),  # Evaluar desde el 10% hasta el 100% de los datos
  measures = list(mmce, setAggregation(mmce, train.mean)),
  resampling = rin_lc,
  show.info = FALSE
)

# Visualizar la curva de aprendizaje
plotLearningCurve(lc_data, facet = "learner")

# --------------------------------------------------
# Paso F: Tuneo del modelo
# --------------------------------------------------
train_data$Es_Cara <- factor(train_data$Es_Cara, levels = c(0, 1), labels = c("No", "Sí"))

test_data$Es_Cara <- factor(test_data$Es_Cara, levels = c(0, 1), labels = c("No", "Sí"))

# Definir la fórmula del modelo
model_formula <- Es_Cara ~ OverallQual + GrLivArea + YearBuilt

# Validación cruzada
cv_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Grid de parámetros para regularización
grid <- expand.grid(
  alpha = c(0, 0.5, 1),
  lambda = 10^seq(-4, 0, length = 10)
)

# Entrenamiento del modelo
set.seed(123)
logistic_reg_tuned <- caret::train(
  model_formula,
  data = train_data,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = grid,
  family = "binomial"
)

# Ver los mejores parámetros encontrados
print(logistic_reg_tuned$bestTune)

# --------------------------------------------------
# Paso G: Matriz de Confusión
# --------------------------------------------------
  # Generar las probabilidades para el conjunto de prueba
test_pred_probs <- predict(logistic_model, newdata = test_data, type = "prob")

# Convertir probabilidades en clases usando el umbral de 0.5
test_pred_class <- ifelse(test_pred_probs[, "1"] > 0.5, "Sí", "No")

# Convertir en factor con los mismos niveles que la variable real
test_pred_class <- factor(test_pred_class, levels = levels(test_data$Es_Cara))

# Calcular la matriz de confusión
conf_matrix_test <- table(Predicted = test_pred_class, Actual = test_data$Es_Cara)
print(conf_matrix_test)

# Usar confusionMatrix para obtener métricas de evaluación
library(caret)
confusion_results <- confusionMatrix(test_pred_class, test_data$Es_Cara)
print(confusion_results)

# Métricas de evaluación
cat("Precisión:", confusion_results$overall['Accuracy'], "\n")
cat("Recall (Sensibilidad):", confusion_results$byClass['Sensitivity'], "\n")
cat("Especificidad:", confusion_results$byClass['Specificity'], "\n")
cat("F1-Score:", confusion_results$byClass['F1'], "\n")

# Medir el tiempo de ejecución del bloque completo
system.time({
  library(profvis)
  
  profvis({
    # Ejecuta el proceso de predicción aquí
    test_pred_probs <- predict(logistic_model, newdata = test_data, type = "prob")
    
    # Pausa de 2 segundos para dar tiempo a profvis
    Sys.sleep(2)
    
    # El proceso posterior para calcular las clases
    test_pred_class <- if_else(test_pred_probs[, 2] > 0.5, 1, 0)
    
    # Pausa de 2 segundos para dar tiempo a profvis
    Sys.sleep(2)
  })
})
# -----------------------------
# Paso H: Comparación de Modelos
# -----------------------------

# 1. Comparar AIC y BIC
cat("=== AIC y BIC ===\n")
cat("Modelo Base:\n")
print(AIC(logistic_model$finalModel))
print(BIC(logistic_model$finalModel))

cat("\nModelo Regularizado (glmnet):\n")
# glmnet no tiene AIC/BIC directamente; extraemos el modelo con coeficientes
# y comparamos con el modelo base
selected_model <- logistic_reg_tuned$finalModel
coeficients <- coef(selected_model, s = logistic_reg_tuned$bestTune$lambda)
print(coeficients)

# 2. Matriz de confusión y métricas (modelo regularizado)
cat("\n=== Matriz de Confusión y Métricas ===\n")

# Predecir clases con modelo regularizado
prob_regularizado <- predict(logistic_reg_tuned, newdata = test_data, type = "prob")[, "Sí"]
class_regularizado <- ifelse(prob_regularizado > 0.5, "Sí", "No")
class_regularizado <- factor(class_regularizado, levels = levels(test_data$Es_Cara))

# Matriz de confusión
conf_matrix_reg <- confusionMatrix(class_regularizado, test_data$Es_Cara)
print(conf_matrix_reg)

# 3. Profiler: comparar tiempos
cat("\n=== Tiempos de Ejecución ===\n")

library(profvis)

cat("Modelo Base:\n")
system.time({
  predict(logistic_model, newdata = test_data, type = "prob")
})

cat("\nModelo Regularizado:\n")
system.time({
  predict(logistic_reg_tuned, newdata = test_data, type = "prob")
})


###---------------------------------------------------
###---------------------------------------------------
# Modelo de clasificación
#---------------------------------------------------
#---------------------------------------------------
library(nnet)

# Asegurarse de que la variable de respuesta esté definida como factor con los niveles correctos
train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))
test_data$PriceCat  <- factor(test_data$PriceCat,  levels = c("Economicas", "Intermedias", "Caras"))

# Definir la fórmula del modelo: en este ejemplo se utilizan las variables OverallQual, GrLivArea y YearBuilt
model_formula <- PriceCat ~ OverallQual + GrLivArea + YearBuilt

# Ajustar el modelo de regresión logística multinomial sobre el conjunto de entrenamiento
multinom_model <- multinom(model_formula, data = train_data)

# Mostrar un resumen del modelo
summary(multinom_model)

# Realizar predicciones en el conjunto de prueba
# Se pueden obtener las probabilidades y la clase asignada:
pred_prob <- predict(multinom_model, newdata = test_data, type = "probs")
pred_class <- predict(multinom_model, newdata = test_data)

# Visualizar la matriz de confusión para evaluar la eficiencia del modelo
cm <- confusionMatrix(as.factor(pred_class), test_data$PriceCat)
print(cm)
# Configurar la estrategia de validación cruzada (por ejemplo, 10-fold CV)


# --------------------------------------------------
# Modelo Multinomial con Validación Cruzada (CV)
# --------------------------------------------------

# Definir la fórmula del modelo: se utilizan las variables OverallQual, GrLivArea y YearBuilt
model_formula <- PriceCat ~ OverallQual + GrLivArea + YearBuilt

# Configurar la estrategia de validación cruzada: 10-fold CV
cv_control <- trainControl(method = "cv", number = 10)

# Ajustar el modelo con CV usando el método "multinom" (del paquete nnet)
set.seed(123)
cv_model <- caret::train(
  model_formula,
  data = train_data,
  method = "multinom",
  trControl = cv_control
)

# Mostrar un resumen del modelo final
cat("Resumen del modelo final:\n")
print(summary(cv_model$finalModel))

# Realizar predicciones sobre el conjunto de prueba
cv_pred <- predict(cv_model, newdata = test_data)

# Calcular la matriz de confusión
cm_cv <- confusionMatrix(cv_pred, test_data$PriceCat)
cat("\nMatriz de Confusión:\n")
print(cm_cv)
# --------------------------------------------------
# Modelo Multinomial con Tuneo de Hiperparámetros y Predicción con la Mejor Combinación
# --------------------------------------------------

# Cargar las librerías necesarias
library(caret)
library(glmnet)
library(ggplot2)

# Asegurarse de que la variable PriceCat esté definida como factor con los niveles correctos
train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))
test_data$PriceCat  <- factor(test_data$PriceCat,  levels = c("Economicas", "Intermedias", "Caras"))

# Definir la fórmula del modelo: se utilizan las variables OverallQual, GrLivArea y YearBuilt
model_formula <- PriceCat ~ OverallQual + GrLivArea + YearBuilt

# Configurar la estrategia de validación cruzada de 10 folds
cv_control <- trainControl(method = "cv", number = 10)

# Definir la rejilla de hiperparámetros para tuneo:
# alpha: 0 (Ridge), 1 (Lasso) o valores intermedios para Elastic Net.
# lambda: secuencia de valores de penalización.
tuneGrid <- expand.grid(
  alpha = c(0, 0.5, 1),
  lambda = seq(0.0001, 0.1, length = 10)
)

# Ajustar el modelo con tuneo utilizando caret y glmnet
set.seed(123)
tuned_model <- caret::train(
  model_formula,
  data = train_data,
  method = "glmnet",
  family = "multinomial",
  trControl = cv_control,
  tuneGrid = tuneGrid
)

# Mostrar el resumen completo del modelo ajustado y la mejor combinación de hiperparámetros
print(tuned_model)
cat("\nMejor combinación de hiperparámetros:\n")
print(tuned_model$bestTune)

# Realizar predicciones sobre el conjunto de prueba utilizando el modelo con la mejor combinación
pred_tuned_best <- predict(tuned_model, newdata = test_data)

# Calcular la matriz de confusión utilizando la mejor configuración
cm_tuned <- confusionMatrix(as.factor(pred_tuned_best), test_data$PriceCat)
print(cm_tuned)


# Extraer resultados del objeto tuneado
tuning_results <- tuned_model$results

# Visualizar con ggplot2: Accuracy vs lambda para cada alpha
library(ggplot2)

ggplot(tuning_results, aes(x = lambda, y = Accuracy, color = as.factor(alpha))) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Evolución de Accuracy en función de lambda y alpha",
       x = "lambda", 
       y = "Accuracy",
       color = "alpha") +
  theme_minimal()

# --------------------------------------------------
# Comparación de los 3 Modelos de Clasificación
# --------------------------------------------------

# (1) Modelo Estándar con multinom() del paquete nnet
library(nnet)
# Ya se ajustó anteriormente:
# multinom_model <- multinom(model_formula, data = train_data)
# Y se obtuvieron:
# pred_class (con multinom_model) y
# cm <- confusionMatrix(as.factor(pred_class), test_data$PriceCat)
# Por ejemplo, la matriz de confusión resultante se muestra con:
print("Matriz de Confusión - Modelo Estándar (multinom):")
print(cm)
accuracy_norm <- cm$overall["Accuracy"]

# (2) Modelo con CV usando caret::train con method = "multinom"
# Ya se ajustó previamente:
# cv_model <- caret::train(model_formula, data = train_data, method = "multinom", trControl = cv_control)
# cv_pred <- predict(cv_model, newdata = test_data)
# cm_cv <- confusionMatrix(cv_pred, test_data$PriceCat)
print("Matriz de Confusión - Modelo con CV:")
print(cm_cv)
accuracy_cv <- cm_cv$overall["Accuracy"]

# (3) Modelo Tuneado con caret y glmnet (family = "multinomial")
# Ya se ajustó previamente:
# tuned_model <- caret::train(model_formula, data = train_data, method = "glmnet",
#                             family = "multinomial", trControl = cv_control, tuneGrid = tuneGrid)
# pred_tuned_best <- predict(tuned_model, newdata = test_data)
# cm_tuned <- confusionMatrix(as.factor(pred_tuned_best), test_data$PriceCat)
print("Matriz de Confusión - Modelo Tuneado:")
print(cm_tuned)
accuracy_tuned <- cm_tuned$overall["Accuracy"]

# Crear un data frame resumen con la métrica Accuracy para cada modelo
accuracy_df <- data.frame(
  Modelo = c("Estándar (multinom)", "Con CV (multinom)", "Tuneado (glmnet)"),
  Accuracy = c(as.numeric(accuracy_norm),
               as.numeric(accuracy_cv),
               as.numeric(accuracy_tuned))
)

print("Resumen de Accuracies:")
print(accuracy_df)



# Visualización: Gráfico de barras comparativo de la Accuracy de cada modelo
library(ggplot2)

ggplot(accuracy_df, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5, size = 3.5) +
  labs(title = "Comparación de Accuracy entre Modelos de Clasificación",
       x = "Modelo",
       y = "Accuracy") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "none")



## ---------------------------------------------------

#===============================================================
# Comparación de Modelos de Clasificación
#===============================================================

# (Si tienes cargado mlr, es recomendable descargarlo para evitar conflictos)
if("package:mlr" %in% search()){
  detach("package:mlr", unload = TRUE)
}

# Cargar las librerías necesarias
library(tidyverse)
library(caret)
library(rpart)         # Para árboles simples
library(rpart.plot)    # Para graficar árboles (opcional)
library(randomForest)
library(e1071)         # Para Naive Bayes (en algunos casos)
library(class)         # Para KNN
library(nnet)          # Para multinom() en Regresión Logística Multinomial
library(reshape2)      # Para melt()
library(RColorBrewer)  # Para paleta en ggplot2

# ------------------------------------------------------------------
# 1. Asegurar consistencia en las variables factor en test_data
# ------------------------------------------------------------------
# Eliminar variables con varianza cero en train_data
cols_to_remove <- nearZeroVar(train_data)
if (length(cols_to_remove) > 0) {
  train_data <- train_data[, -cols_to_remove, drop = FALSE]
  
  # Asegurar que test_data también elimine las mismas columnas
  common_cols <- intersect(names(train_data), names(test_data))
  test_data <- test_data[, common_cols, drop = FALSE]
}

# Manejo de valores NA en datos numéricos: imputar con mediana (usando la mediana de train_data)
numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
for (col in numeric_cols) {
  train_data[[col]][is.na(train_data[[col]])] <- median(train_data[[col]], na.rm = TRUE)
  test_data[[col]][is.na(test_data[[col]])] <- median(train_data[[col]], na.rm = TRUE)
}

# Para las variables factor, imputar con el modo (valor más frecuente)
factor_cols <- names(train_data)[sapply(train_data, is.factor)]
for (col in factor_cols) {
  mode_value <- names(sort(table(train_data[[col]]), decreasing = TRUE))[1]
  train_data[[col]][is.na(train_data[[col]])] <- mode_value
  test_data[[col]][is.na(test_data[[col]])] <- mode_value
}

# Forzar que para cada variable factor en train_data, test_data tenga exactamente los mismos niveles
for (var in factor_cols) {
  if (var %in% names(test_data)) {
    new_levels <- setdiff(unique(test_data[[var]]), levels(train_data[[var]]))
    if (length(new_levels) > 0) {
      cat("Warning: La variable", var, "tiene niveles nuevos en test_data:", new_levels, "\n")
      # Reemplazar los niveles nuevos por NA
      test_data[[var]][ test_data[[var]] %in% new_levels ] <- NA
    }
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}

# ------------------------------------------------------------------
# 2. Definir los predictores (No usaremos SalePrice, pues PriceCat se deriva de él)
# ------------------------------------------------------------------
predictors <- setdiff(names(train_data), c("SalePrice", "PriceCat"))

# ------------------------------------------------------------------
# 3. Entrenar los diferentes modelos
# ------------------------------------------------------------------

### 3.1. Modelo de Árbol de Clasificación Base (rpart)
formula_class <- PriceCat ~ . - SalePrice
modelo_class_base <- rpart(formula_class, data = train_data, method = "class")
pred_tree_base <- predict(modelo_class_base, newdata = test_data, type = "class")
cm_tree_base <- confusionMatrix(pred_tree_base, test_data$PriceCat)

### 3.3. Random Forest para Clasificación
# Asegurarse de que los nombres de las columnas sean válidos en R
names(train_data) <- make.names(names(train_data))
names(test_data) <- make.names(names(test_data))

set.seed(123)
modelo_rf <- randomForest(PriceCat ~ . - SalePrice, data = train_data, na.action = na.omit)
pred_rf <- predict(modelo_rf, newdata = test_data)
cm_rf <- confusionMatrix(pred_rf, test_data$PriceCat)

### 3.4. Naive Bayes para Clasificación
names(train_data) <- make.names(names(train_data))
names(test_data)  <- make.names(names(test_data))

# Volver a definir los predictores a partir de los nombres actualizados.
predictors <- setdiff(names(train_data), c("SalePrice", "PriceCat"))

# Ajustar el modelo Naive Bayes utilizando los predictors actualizados
nb_model_class <- naiveBayes(PriceCat ~ . - SalePrice, data = train_data)
pred_nb <- predict(nb_model_class, newdata = test_data[, predictors])
cm_nb <- confusionMatrix(pred_nb, test_data$PriceCat)
print(cm_nb)

### 3.5. KNN para Clasificación (modelo simple)
numeric_predictors <- predictors[sapply(train_data[, predictors], is.numeric)]
x_train <- scale(train_data[, numeric_predictors])
x_test  <- scale(test_data[, numeric_predictors])
y_train <- train_data$PriceCat
y_test  <- test_data$PriceCat
k_simple <- round(sqrt(nrow(train_data)), 0)
pred_knn <- knn(x_train, x_test, y_train, k = k_simple)
cm_knn <- confusionMatrix(pred_knn, y_test)

### 3.6. Regresión Logística Multinomial (modelo estándar)
# Usamos la fórmula: PriceCat ~ OverallQual + GrLivArea + YearBuilt
model_formula <- PriceCat ~ OverallQual + GrLivArea + YearBuilt
logistic_model <- multinom(model_formula, data = train_data)
pred_logistic <- predict(logistic_model, newdata = test_data)
cm_logistic <- confusionMatrix(as.factor(pred_logistic), test_data$PriceCat)

### 3.7. Modelo Multinomial con Tuneo de Hiperparámetros y Validación Cruzada (usando glmnet)
# Reafirmar que PriceCat tenga los niveles correctos
train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))
test_data$PriceCat  <- factor(test_data$PriceCat,  levels = c("Economicas", "Intermedias", "Caras"))
model_formula <- PriceCat ~ OverallQual + GrLivArea + YearBuilt
cv_control <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(
  alpha = c(0, 0.5, 1),
  lambda = seq(0.0001, 0.1, length = 10)
)
set.seed(123)
tuned_model <- caret::train(
  model_formula,
  data = train_data,
  method = "glmnet",
  family = "multinomial",
  trControl = cv_control,
  tuneGrid = tuneGrid
)
cat("Mejor combinación de hiperparámetros (modelo tuneado):\n")
print(tuned_model$bestTune)
pred_tuned_best <- predict(tuned_model, newdata = test_data)
cm_tuned <- confusionMatrix(as.factor(pred_tuned_best), test_data$PriceCat)

# ------------------------------------------------------------------
# 4. Función para calcular el F1-score promedio a partir de la matriz de confusión
# ------------------------------------------------------------------
calcular_f1_promedio <- function(cm) {
  if (is.matrix(cm$byClass)) {
    return(mean(cm$byClass[,"F1"], na.rm = TRUE))
  } else {
    return(cm$byClass["F1"])
  }
}

# ------------------------------------------------------------------
# 5. Extraer las métricas de cada modelo
# ------------------------------------------------------------------
metrics_df <- data.frame(
  Model = c("Tree Base", "Random Forest", "Naive Bayes", "KNN", "Logistic Regression", "Tuned (glmnet)"),
  Accuracy = c(as.numeric(cm_tree_base$overall["Accuracy"]),
               as.numeric(cm_rf$overall["Accuracy"]),
               as.numeric(cm_nb$overall["Accuracy"]),
               as.numeric(cm_knn$overall["Accuracy"]),
               as.numeric(cm_logistic$overall["Accuracy"]),
               as.numeric(cm_tuned$overall["Accuracy"])),
  Kappa = c(as.numeric(cm_tree_base$overall["Kappa"]),
            as.numeric(cm_rf$overall["Kappa"]),
            as.numeric(cm_nb$overall["Kappa"]),
            as.numeric(cm_knn$overall["Kappa"]),
            as.numeric(cm_logistic$overall["Kappa"]),
            as.numeric(cm_tuned$overall["Kappa"])),
  F1 = c(calcular_f1_promedio(cm_tree_base),
         calcular_f1_promedio(cm_rf),
         calcular_f1_promedio(cm_nb),
         calcular_f1_promedio(cm_knn),
         calcular_f1_promedio(cm_logistic),
         calcular_f1_promedio(cm_tuned))
)
cat("Comparación de métricas de clasificación:\n")
print(metrics_df)

# ------------------------------------------------------------------
# 6. Graficar la comparación de Accuracy y F1-score
# ------------------------------------------------------------------
metrics_melt <- melt(metrics_df, id.vars = "Model", variable.name = "Metric", value.name = "Value")
ggplot(metrics_melt, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Modelos de Clasificación",
       y = "Valor de la Métrica") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")
