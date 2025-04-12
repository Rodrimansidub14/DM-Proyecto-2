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




