# Cargar librerías necesarias
library(caret)        # Para modelado y evaluación (train, confusionMatrix, etc.)
library(rpart)        # Para árboles de decisión
library(rpart.plot)   # Para visualizar árboles
library(randomForest) # Para Random Forest
library(e1071)        # Para Naive Bayes
library(class)        # Para KNN
library(dplyr)        # Para manipulación de datos
library(reshape2)     # Para reorganizar data frames en formato largo (melt)
library(ggplot2)      # Para gráficos

# Se asume que train_data y test_data ya han sido cargados y preprocesados,
# y que se ha creado la variable de clasificación PriceCat en ambos conjuntos.
# Por ejemplo:
#   train_data <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = TRUE)
#   test_data  <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = TRUE)
#   ... (creación de PriceCat usando los cuartiles de SalePrice)

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
pred_tree_cv <- predict(modelo_class_cv, newdata = test_data, type = "class")
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
