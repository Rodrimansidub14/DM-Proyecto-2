# ====================================================
# Script: Comparación de Modelos de Regresión con Métricas Adicionales
# ====================================================
# Se comparan cuatro modelos:
#   1. Naive Bayes (adaptado para regresión mediante discretización)
#   2. Regresión Lineal (modelo stepwise)
#   3. Árbol de Regresión (modelo base)
#   4. Random Forest
#
# Se evalúan con las siguientes métricas: RMSE, MAE, MAPE y R², y se
# generan gráficos comparativos de “Predicción vs. Valor Real” (por facetas).
#
# ====================================================
# Sección 1: Cargar librerías y Datos
# ====================================================
library(e1071)      # Para naiveBayes
library(dplyr)      # Para manipulación de datos
library(caret)      # Para RMSE, validación y Random Forest
library(ggplot2)    # Para gráficos
library(rpart)      # Para Árbol de Regresión
library(rpart.plot) # Para visualizar árboles

# Cargar datos preprocesados
train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)

# Asegurar consistencia: ajustar niveles de los factores en test_data
factor_vars <- names(train_data)[sapply(train_data, is.factor)]
for (var in factor_vars) {
  if (var %in% names(test_data)) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}

# Eliminar filas con NA en ambos conjuntos
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]

# Definir los predictores: todas las variables excepto "SalePrice"
predictors <- setdiff(names(train_data), "SalePrice")

# Para modelos que usan los datos originales (Regresión Lineal, Árbol, RF)
train_filtered <- train_data[, !(names(train_data) %in% "SalesPrice_bin")]
test_filtered  <- test_data[, !(names(test_data) %in% "SalesPrice_bin")]

# ====================================================
# Sección 2: Modelo de Naive Bayes para Regresión
# ====================================================
# Objetivo: Predecir SalesPrice usando Naive Bayes adaptado.
# Se discretiza SalesPrice y se estima el valor esperado.

# 2.1. Discretizar SalesPrice en el conjunto de entrenamiento
n_bins <- 50  
unique_vals <- length(unique(train_data$SalePrice))
n_bins <- min(n_bins, unique_vals - 1)
# Usar cortes basados en quantiles para obtener bins con frecuencia similar
bins <- quantile(train_data$SalePrice, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
bins <- unique(bins)  # Evitar cortes repetidos
train_data$SalesPrice_bin <- cut(train_data$SalePrice, breaks = bins, include.lowest = TRUE, dig.lab = 10)

# 2.2. Calcular los centros de cada intervalo a partir de los cortes
bin_centers <- (head(bins, -1) + tail(bins, -1)) / 2
cat("Centros de cada bin (Naive Bayes):\n")
print(bin_centers)

# 2.3. Entrenar el modelo Naive Bayes (regresión) usando la variable discretizada
nb_model_reg <- naiveBayes(SalesPrice_bin ~ ., data = train_data[, c(predictors, "SalesPrice_bin")])

# 2.4. Predecir en el conjunto de prueba: obtener probabilidades para cada bin
nb_pred_probs <- predict(nb_model_reg, newdata = test_data[, predictors], type = "raw")

# 2.5. Calcular la predicción final como el valor esperado
nb_pred_reg <- apply(nb_pred_probs, 1, function(prob_vec) sum(prob_vec * bin_centers))

# 2.6. Calcular el RMSE para el modelo de Naive Bayes
rmse_nb <- RMSE(nb_pred_reg, test_data$SalePrice)
cat("RMSE del Modelo Naive Bayes (Regresión):", rmse_nb, "\n")




# ====================================================
# Sección 3: Modelo de Regresión Lineal (Stepwise)
# ====================================================
# Se ajusta un modelo lineal multivariado usando la técnica stepwise.
modelo_stepwise <- step(
  lm(SalePrice ~ ., data = train_filtered),
  direction = "backward",
  scope = list(upper = ~ ., lower = ~ 1),
  trace = FALSE
)
summary(modelo_stepwise)

# Predicción en el conjunto de prueba
pred_lin <- predict(modelo_stepwise, newdata = test_filtered)
rmse_lin <- RMSE(pred_lin, test_filtered$SalePrice)
cat("RMSE del Modelo de Regresión Lineal (Stepwise):", rmse_lin, "\n")

# ====================================================
# Sección 4: Modelo de Árbol de Regresión (Base)
# ====================================================
# Se ajusta un árbol de regresión usando rpart (método "anova")
tree_model <- rpart(SalePrice ~ ., data = train_filtered, method = "anova")
# (Opcional: visualizar el árbol)
rpart.plot(tree_model, main = "Árbol de Regresión: Modelo Base")
pred_tree <- predict(tree_model, newdata = test_filtered)
rmse_tree <- RMSE(pred_tree, test_filtered$SalePrice)
cat("RMSE del Árbol de Regresión (Modelo Base):", rmse_tree, "\n")

# ====================================================
# Sección 5: Modelo de Random Forest
# ====================================================
set.seed(123)
control_cv_reg <- trainControl(method = "cv", number = 10)
grid_rf <- expand.grid(mtry = c(2, 4, 6, 8))
rf_model <- caret::train(SalePrice ~ ., 
                         data = train_filtered, 
                         method = "rf",
                         trControl = control_cv_reg,
                         tuneGrid = grid_rf,
                         metric = "RMSE")
pred_rf <- predict(rf_model, newdata = test_filtered)
rmse_rf <- RMSE(pred_rf, test_filtered$SalePrice)
cat("RMSE del Random Forest:", rmse_rf, "\n")

# ====================================================
# Sección 6: Cálculo de Métricas Adicionales y Comparación Final
# ====================================================
# Definimos una función para calcular varias métricas:
calculate_metrics <- function(actual, predicted) {
  rmse <- RMSE(predicted, actual)
  mae <- mean(abs(predicted - actual))
  mape <- mean(abs((predicted - actual)/actual)) * 100
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(data.frame(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2))
}

metrics_nb <- calculate_metrics(test_data$SalePrice, nb_pred_reg)
metrics_lin <- calculate_metrics(test_filtered$SalePrice, pred_lin)
metrics_tree <- calculate_metrics(test_filtered$SalePrice, pred_tree)
metrics_rf <- calculate_metrics(test_filtered$SalePrice, pred_rf)

df_metrics <- rbind(
  cbind(Modelo = "Naive Bayes", metrics_nb),
  cbind(Modelo = "Regresión Lineal (Stepwise)", metrics_lin),
  cbind(Modelo = "Árbol de Regresión", metrics_tree),
  cbind(Modelo = "Random Forest", metrics_rf)
)
cat("\nComparación Final de Métricas:\n")
print(df_metrics)


# ====================================================
# Sección 7: Gráficos Comparativos
# ====================================================
# Crear data frames individuales para cada modelo
df_nb <- data.frame(Actual = test_data$SalePrice, Predicted = nb_pred_reg, Modelo = "Naive Bayes")
df_lin <- data.frame(Actual = test_filtered$SalePrice, Predicted = pred_lin, Modelo = "Regresión Lineal (Stepwise)")
df_tree <- data.frame(Actual = test_filtered$SalePrice, Predicted = pred_tree, Modelo = "Árbol de Regresión")
df_rf <- data.frame(Actual = test_filtered$SalePrice, Predicted = pred_rf, Modelo = "Random Forest")

df_completo <- rbind(df_nb, df_lin, df_tree, df_rf)

# Gráfico por facetas: cada modelo en su panel
ggplot(df_completo, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ Modelo, scales = "free") +
  labs(title = "Predicción vs. Valor Real por Modelo",
       x = "Valor Real (SalePrice)",
       y = "Valor Predicho") +
  theme_minimal()

# Gráfico combinado: todos los modelos en un mismo panel
ggplot(df_completo, aes(x = Actual, y = Predicted, color = Modelo)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Comparación de Predicción vs. Valor Real: NB, Lineal, Árbol y RF",
       x = "Valor Real (SalePrice)",
       y = "Valor Predicho") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ====================================================
# Sección 4: Creación de la Variable de Clasificación PriceCat
# ====================================================
# Usamos los cuartiles para definir los cortes:
#   - "Economicas": SalePrice < primer cuartil
#   - "Intermedias": primer cuartil <= SalePrice < tercer cuartil
#   - "Caras": SalePrice >= tercer cuartil

cat("\nResumen de SalePrice en train:\n")
print(summary(train_data$SalePrice))

# Calcular umbrales basados en cuartiles
cuartiles <- quantile(train_data$SalePrice, probs = c(0.25, 0.75), na.rm = TRUE)
lower_threshold <- cuartiles[1]
upper_threshold <- cuartiles[2]

cat("\nUmbrales elegidos para clasificar:\n")
cat("Economicas: < ", lower_threshold, "\n")
cat("Intermedias: [", lower_threshold, ", ", upper_threshold, ")\n")
cat("Caras: >= ", upper_threshold, "\n\n")

# Crear la variable categórica PriceCat en el conjunto de entrenamiento
train_data$PriceCat <- dplyr::case_when(
  train_data$SalePrice < lower_threshold ~ "Economicas",
  train_data$SalePrice < upper_threshold ~ "Intermedias",
  TRUE ~ "Caras"
)
train_data$PriceCat <- factor(train_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))

# Crear la misma variable en el conjunto de prueba
test_data$PriceCat <- dplyr::case_when(
  test_data$SalePrice < lower_threshold ~ "Economicas",
  test_data$SalePrice < upper_threshold ~ "Intermedias",
  TRUE ~ "Caras"
)
test_data$PriceCat <- factor(test_data$PriceCat, levels = c("Economicas", "Intermedias", "Caras"))

cat("Distribución de PriceCat en train:\n")
print(table(train_data$PriceCat))
cat("\nDistribución de PriceCat en test:\n")
print(table(test_data$PriceCat))


# ====================================================
# Sección 5: Modelo de Clasificación con Naive Bayes (e1071)
# ====================================================
# Se entrena un modelo de clasificación usando la función naiveBayes
# Se elimina SalePrice de los predictores porque PriceCat se derivó de él.

# Asegurarse de que 'predictors' no incluya "SalePrice" (ya se definió en la Sección 1)
nb_model_class <- naiveBayes(PriceCat ~ . - SalePrice, data = train_data)

# Predicción en el conjunto de prueba (usando únicamente los predictores)
nb_pred_class <- predict(nb_model_class, newdata = test_data[, predictors])

# Evaluación del modelo mediante matriz de confusión
cm_nb_class <- confusionMatrix(nb_pred_class, test_data$PriceCat)
cat("\nMatriz de Confusión y Métricas del Modelo Naive Bayes (Clasificación):\n")
print(cm_nb_class)



# ====================================================
#Sección 7 : Analice el modelo. ¿Cree que pueda estar sobreajustado?
# ====================================================
# ====================================================
# Sección 7: Análisis de Sobreajuste en el Modelo de Clasificación
# ====================================================

# Evaluación en el conjunto de entrenamiento:
nb_pred_train <- predict(nb_model_class, newdata = train_data[, predictors])
cm_train <- confusionMatrix(nb_pred_train, train_data$PriceCat)
cat("\nMatriz de Confusión y Métricas en Entrenamiento:\n")
print(cm_train)

# Evaluación en el conjunto de prueba:
nb_pred_test <- predict(nb_model_class, newdata = test_data[, predictors])
cm_test <- confusionMatrix(nb_pred_test, test_data$PriceCat)
cat("\nMatriz de Confusión y Métricas en Prueba:\n")
print(cm_test)

# Comparación de la Accuracy:
train_accuracy <- cm_train$overall["Accuracy"]
test_accuracy  <- cm_test$overall["Accuracy"]

cat("\nComparación de Accuracy:\n")
cat("Accuracy en entrenamiento:", train_accuracy, "\n")
cat("Accuracy en prueba:", test_accuracy, "\n")

# Si el accuracy en entrenamiento es muy alto (por ejemplo, cerca de 100%)
# y el accuracy en prueba es significativamente menor, es indicio de sobreajuste.

# También graficamos la comparación de Accuracy:
accuracy_df <- data.frame(
  Conjunto = c("Entrenamiento", "Prueba"),
  Accuracy = c(as.numeric(train_accuracy), as.numeric(test_accuracy))
)

ggplot(accuracy_df, aes(x = Conjunto, y = Accuracy, fill = Conjunto)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  labs(title = "Comparación de Accuracy: Entrenamiento vs. Prueba",
       y = "Accuracy") +
  theme_minimal()


# ====================================================
#Haga un modelo usando validación cruzada, compare los resultados de este con los delmodelo anterior. ¿Cuál funcionó mejor?

# ====================================================
# ====================================================
# Sección 8: Modelo de Naive Bayes con Validación Cruzada y Comparación
# ====================================================
# ====================================================
# Comparar Naive Bayes con y sin Validación Cruzada
# ====================================================

# Cargar librerías necesarias
library(e1071)      # Para naiveBayes
library(caret)      # Para RMSE, validación cruzada y otros
library(dplyr)      # Para manipulación de datos

# Cargar datos preprocesados
train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)

factor_vars <- names(train_data)[sapply(train_data, is.factor)]
for (var in factor_vars) {
  if (var %in% names(test_data)) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}

# Eliminar filas con NA en ambos conjuntos
train_data <- train_data[complete.cases(train_data), ]
test_data  <- test_data[complete.cases(test_data), ]

# Definir los predictores: todas las variables excepto "SalePrice"
predictors <- setdiff(names(train_data), "SalePrice")

# Definir los bins para la variable "SalePrice" (discretización)
n_bins <- 50
unique_vals <- length(unique(train_data$SalePrice))
n_bins <- min(n_bins, unique_vals - 1)
bins <- quantile(train_data$SalePrice, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
bins <- unique(bins)  # Evitar cortes repetidos
train_data$SalesPrice_bin <- cut(train_data$SalePrice, breaks = bins, include.lowest = TRUE, dig.lab = 10)
bin_centers <- (head(bins, -1) + tail(bins, -1)) / 2

# ====================================================
# Versión sin Validación Cruzada
# ====================================================

# Entrenar el modelo Naive Bayes sin validación cruzada
nb_model_no_cv <- naiveBayes(SalesPrice_bin ~ ., data = train_data[, c(predictors, "SalesPrice_bin")])

# Predecir en el conjunto de prueba sin validación cruzada
nb_pred_probs_no_cv <- predict(nb_model_no_cv, newdata = test_data[, predictors], type = "raw")

# Calcular las predicciones como valor esperado
nb_pred_no_cv <- apply(nb_pred_probs_no_cv, 1, function(prob_vec) sum(prob_vec * bin_centers))

# Calcular el RMSE para el modelo sin validación cruzada
rmse_no_cv <- RMSE(nb_pred_no_cv, test_data$SalePrice)
cat("RMSE sin validación cruzada:", rmse_no_cv, "\n")

# ====================================================
# Versión con Validación Cruzada
# ====================================================

control_cv <- trainControl(method = "cv", number = 10)

# Entrenar el modelo Naive Bayes con validación cruzada
grid <- expand.grid(laplace = c(0, 1, 2),
                    usekernel = c(TRUE, FALSE),
                    adjust = c(0.5, 1, 2))

results_cv <- data.frame(laplace = numeric(),
                         usekernel = logical(),
                         adjust = numeric(),
                         RMSE = numeric())

for (i in 1:nrow(grid)) {
  params <- grid[i, ]
  cat("Evaluando con validación cruzada: laplace =", params$laplace,
      " usekernel =", params$usekernel,
      " adjust =", params$adjust, "\n")
  
  nb_model_cv <- train(SalesPrice_bin ~ .,
                       data = train_data[, c(predictors, "SalesPrice_bin")],
                       method = "naive_bayes",
                       trControl = control_cv,
                       tuneGrid = params)
  
  # Predecir en el conjunto de prueba con el modelo de validación cruzada
  nb_pred_probs_cv <- predict(nb_model_cv, newdata = test_data[, predictors], type = "prob")
  
  nb_pred_cv <- apply(nb_pred_probs_cv, 1, function(prob_vec) sum(prob_vec * bin_centers))
  
  # Calcular el RMSE para esta combinación
  rmse_cv <- RMSE(nb_pred_cv, test_data$SalePrice)
  cat("RMSE con validación cruzada:", rmse_cv, "\n")
  
  # Almacenar el resultado
  results_cv <- rbind(results_cv, cbind(params, RMSE = rmse_cv))
}

# Mostrar los resultados ordenados por RMSE de la validación cruzada
results_cv <- results_cv[order(results_cv$RMSE), ]
cat("Resultados de la búsqueda de hiperparámetros con validación cruzada:\n")
print(results_cv)

# ====================================================
# Comparación de Resultados
# ====================================================

cat("\nComparación de RMSE:\n")
cat("RMSE sin validación cruzada:", rmse_no_cv, "\n")
cat("Mejor RMSE con validación cruzada:", min(results_cv$RMSE), "\n")



# ====================================================
# Sección 10: Comparación de Modelos de Regresión con Métricas Adicionales
# ====================================================

# ====================================================
# Script: Comparación de Modelos de Clasificación
# ====================================================
# Se comparan tres modelos de clasificación:
#   1. Árbol de Decisión Tuned (usando rpart2 y validación cruzada)
#   2. Random Forest
#   3. Naïve Bayes
#
# Se evalúan utilizando la matriz de confusión (accuracy, Kappa, etc.)
# y se grafican los resultados para comparar su desempeño.

# Cargar librerías necesarias
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# ----------------------------
# Asumimos que:
# - train_data y test_data ya están cargados, limpios y con la variable PriceCat creada.
# - 'predictors' es un vector con el nombre de todas las variables predictoras (excluyendo "SalePrice").
# - La fórmula para clasificación es:
formula_class <- PriceCat ~ . - SalePrice

# ----------------------------
# Configurar validación cruzada para el modelo de árbol
# ----------------------------
control_cv <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

# ----------------------------
# Modelo 1: Árbol de Decisión Tuned (usando rpart2 y CV)
# ----------------------------
set.seed(123)
# Entrenar el modelo usando caret y variando la profundidad máxima de 2 a 10
modelo_class_cv <- caret::train(formula_class, 
                                data = train_data, 
                                method = "rpart2",      
                                tuneGrid = expand.grid(maxdepth = 2:10),
                                trControl = control_cv,
                                metric = "Accuracy")
rpart.plot(modelo_class_cv$finalModel, main = "Árbol de Clasificación Tuned (PriceCat)")

# Predicción en el conjunto de prueba
pred_tree_class <- predict(modelo_class_cv, newdata = test_data)

# ----------------------------
# Modelo 2: Random Forest para Clasificación
# ----------------------------
set.seed(123)
modelo_rf <- randomForest(PriceCat ~ . - SalePrice, data = train_data, na.action = na.omit)
ypred_rf <- predict(modelo_rf, newdata = test_data)
ypred_rf <- factor(ypred_rf, levels = levels(train_data$PriceCat))

# ----------------------------
# Modelo 3: Naïve Bayes para Clasificación
# ----------------------------
nb_model_class <- naiveBayes(PriceCat ~ . - SalePrice, data = train_data)
nb_pred_class <- predict(nb_model_class, newdata = test_data[, predictors])

# ----------------------------
# Evaluación: Matrices de Confusión para cada modelo
# ----------------------------
cm_tree <- confusionMatrix(pred_tree_class, test_data$PriceCat)
cm_rf   <- confusionMatrix(ypred_rf, test_data$PriceCat)
cm_nb   <- confusionMatrix(nb_pred_class, test_data$PriceCat)

# Mostrar cada matriz de confusión
cat("\nMatriz de Confusión - Árbol de Decisión Tuned:\n")
print(cm_tree)

cat("\nMatriz de Confusión - Random Forest:\n")
print(cm_rf)

cat("\nMatriz de Confusión - Naïve Bayes:\n")
print(cm_nb)

# ----------------------------
# Extraer métricas globales: Accuracy y Kappa
# ----------------------------
metrics_class <- data.frame(
  Modelo   = c("Árbol Tuned", "Random Forest", "Naïve Bayes"),
  Accuracy = c(as.numeric(cm_tree$overall["Accuracy"]),
               as.numeric(cm_rf$overall["Accuracy"]),
               as.numeric(cm_nb$overall["Accuracy"])),
  Kappa    = c(as.numeric(cm_tree$overall["Kappa"]),
               as.numeric(cm_rf$overall["Kappa"]),
               as.numeric(cm_nb$overall["Kappa"]))
)

cat("\nComparación de Métricas Globales para Modelos de Clasificación:\n")
print(metrics_class)
knitr::kable(metrics_class, 
             caption = "Comparación de Métricas Globales (Accuracy y Kappa)",
             align = "c") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# ----------------------------
# Gráfico comparativo de Accuracy
# ----------------------------
ggplot(metrics_class, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  labs(title = "Comparación de Accuracy: Modelos de Clasificación",
       y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### KNN con mejor k


