# RNA Classification Script: Puntos 1-8
# Universidad del Valle de Guatemala - Minería de Datos CC3074 - Proyecto 2 - Entrega 7

# ------------------------------
# Punto 1: Cargar librerías y datos
# ------------------------------
library(tidyverse)
library(caret)
library(nnet)
library(neuralnet)
set.seed(123)

# Rutas de los archivos
data_dir <- "C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed"
train_path <- file.path(data_dir, "train_preprocessed.csv")
test_path  <- file.path(data_dir, "test_preprocessed.csv")

train_raw <- read.csv(train_path, stringsAsFactors = TRUE)
test_raw  <- read.csv(test_path,  stringsAsFactors = TRUE)

# ------------------------------
# Punto 2: Crear variable PriceCat según cuartiles de SalePrice
# ------------------------------
cuartiles       <- quantile(train_raw$SalePrice, probs = c(0.25, 0.75))
lower_threshold <- cuartiles[1]
upper_threshold <- cuartiles[2]

train_raw <- train_raw %>%
  mutate(PriceCat = case_when(
    SalePrice < lower_threshold ~ "Economicas",
    SalePrice < upper_threshold ~ "Intermedias",
    TRUE                         ~ "Caras"
  )) %>%
  mutate(PriceCat = factor(PriceCat, levels = c("Economicas","Intermedias","Caras")))

test_raw <- test_raw %>%
  mutate(PriceCat = case_when(
    SalePrice < lower_threshold ~ "Economicas",
    SalePrice < upper_threshold ~ "Intermedias",
    TRUE                         ~ "Caras"
  )) %>%
  mutate(PriceCat = factor(PriceCat, levels = c("Economicas","Intermedias","Caras")))

# ------------------------------
# Punto 3: Preprocesamiento general
# ------------------------------
# Quitar Id y SalePrice
drop_cols <- c("Id","SalePrice")
train_data <- train_raw %>% select(-all_of(drop_cols))
test_data  <- test_raw  %>% select(-all_of(drop_cols))

# Alinear niveles de factores en test_data a los de train_data
auto_align_factors <- function(train_df, test_df) {
  facs <- names(train_df)[sapply(train_df, is.factor)]
  for(v in facs) test_df[[v]] <- factor(test_df[[v]], levels = levels(train_df[[v]]))
  test_df
}
test_data <- auto_align_factors(train_data, test_data)

# One-hot encoding de categóricas (excluye PriceCat)
dummies     <- dummyVars(~ . - PriceCat, data = train_data, fullRank = TRUE)
train_dummy <- predict(dummies, newdata = train_data) %>% as.data.frame()
test_dummy  <- predict(dummies, newdata = test_data)  %>% as.data.frame()
train_dummy$PriceCat <- train_data$PriceCat
test_dummy$PriceCat  <- test_data$PriceCat

# Escalado e imputación de predictores
preproc <- preProcess(train_dummy %>% select(-PriceCat), method = c("medianImpute","center","scale"))
train_pp <- predict(preproc, train_dummy %>% select(-PriceCat)) %>% as.data.frame()
test_pp  <- predict(preproc, test_dummy  %>% select(-PriceCat)) %>% as.data.frame()
train_pp$PriceCat <- train_dummy$PriceCat
test_pp$PriceCat  <- test_dummy$PriceCat

# Verificar dimensiones y distribución de PriceCat
cat("Dimensiones train:", dim(train_pp), "\n")
cat("Dimensiones test: ", dim(test_pp),  "\n")
print(table(train_pp$PriceCat))
print(table(test_pp$PriceCat))

# ------------------------------
# Punto 4: Generar dos modelos de RNA de clasificación
# ------------------------------
# Modelo 1: nnet con 5 neuronas ocultas y decay=0.1 (aumentando MaxNWts)
ctrl1 <- trainControl(method = "cv", number = 5)
grid1 <- expand.grid(size = 5, decay = 0.1)
time1 <- system.time({
  model1 <- train(PriceCat ~ ., data = train_pp,
                  method    = "nnet",
                  trControl = ctrl1,
                  tuneGrid  = grid1,
                  MaxNWts   = 5000,
                  trace     = FALSE,
                  maxit     = 200)
})


# 4.1 Construir matriz de respuesta dummy y dataset
resp_dum <- class.ind(train_pp$PriceCat)
colnames(resp_dum) <- paste0("Y_", levels(train_pp$PriceCat))

# quitar PriceCat y unir
train_nn <- cbind(train_pp %>% select(-PriceCat), resp_dum)

# 4.2 Asegurar que es data.frame y nombres válidos
train_nn <- as.data.frame(train_nn)
names(train_nn) <- make.names(names(train_nn))

# 4.3 Reconstruir la fórmula con los nuevos nombres
outcome_vars <- make.names(colnames(resp_dum))
predictor_vars <- make.names(names(train_pp)[names(train_pp) != "PriceCat"])
f_nn <- as.formula(paste(
  paste(outcome_vars, collapse = " + "),
  "~",
  paste(predictor_vars, collapse = " + ")
))

# 4.4 Entrenar la red
time2 <- system.time({
  model2 <- neuralnet(
    formula       = f_nn,
    data          = train_nn,
    hidden        = c(4,2),
    act.fct       = "tanh",
    linear.output = FALSE
  )
})


# ------------------------------
# Punto 5: Predicción y matrices de confusión
# ------------------------------
pred1 <- predict(model1, newdata = test_pp)
conf1 <- confusionMatrix(pred1, test_pp$PriceCat)

raw2 <- compute(model2, test_pp %>% select(-PriceCat))$net.result
pred2 <- factor(apply(raw2, 1, function(x) levels(test_pp$PriceCat)[which.max(x)]),
                levels = levels(test_pp$PriceCat))
conf2 <- confusionMatrix(pred2, test_pp$PriceCat)

print(conf1)
print(conf2)

# ------------------------------
# Punto 6: Comparar resultados (Accuracy, Kappa, Tiempo)
# ------------------------------
results <- tibble(
  Modelo   = c("nnet (5,0.1)", "neuralnet (4-2)"),
  Accuracy = c(conf1$overall["Accuracy"], conf2$overall["Accuracy"]),
  Kappa    = c(conf1$overall["Kappa"],    conf2$overall["Kappa"]),
  Tiempo   = c(time1["elapsed"],          time2["elapsed"])
)
print(results)

# ------------------------------
# Punto 7: Analizar sobreajuste (Train vs Test)
# ------------------------------
train_pred1 <- predict(model1, train_pp)
train_conf1 <- confusionMatrix(train_pred1, train_pp$PriceCat)
raw_tr2    <- compute(model2, train_pp %>% select(-PriceCat))$net.result
train_pred2 <- factor(apply(raw_tr2, 1, function(x) levels(train_pp$PriceCat)[which.max(x)]),
                      levels = levels(train_pp$PriceCat))
train_conf2 <- confusionMatrix(train_pred2, train_pp$PriceCat)

cat("Model1 Train vs Test Accuracies:", train_conf1$overall["Accuracy"], "vs", conf1$overall["Accuracy"], "\n")
cat("Model2 Train vs Test Accuracies:", train_conf2$overall["Accuracy"], "vs", conf2$overall["Accuracy"], "\n")

# ------------------------------
# Punto 8: Tuneo del modelo elegido (Model1)
# ------------------------------
ctrl2 <- trainControl(method = "cv", number = 5)
grid2 <- expand.grid(size = c(3,5,7), decay = c(0,0.001,0.01))
model1_tuned <- train(PriceCat ~ ., data = train_pp,
                      method    = "nnet",
                      trControl = ctrl2,
                      tuneGrid  = grid2,
                      MaxNWts   = 5000,
                      trace     = FALSE,
                      maxit     = 200)

print(model1_tuned$bestTune)
best_pred <- predict(model1_tuned, test_pp)
best_conf <- confusionMatrix(best_pred, test_pp$PriceCat)
print(best_conf)
# ------------------------------
# Puntos 9 y 10: Modelos de regresión con RNA
# ------------------------------

# Preparar datos para regresión
drop_cols <- c("Id", "PriceCat")  # eliminar categoría
train_reg <- train_raw %>% select(-all_of(drop_cols))
test_reg  <- test_raw  %>% select(-all_of(drop_cols))

# Alinear factores categóricos
test_reg <- auto_align_factors(train_reg, test_reg)

# One-hot encoding
dummies_reg <- dummyVars(SalePrice ~ ., data = train_reg, fullRank = TRUE)
train_dummy_r <- predict(dummies_reg, newdata = train_reg) %>% as.data.frame()
test_dummy_r  <- predict(dummies_reg, newdata = test_reg)  %>% as.data.frame()

# Escalado e imputación
preproc_r <- preProcess(train_dummy_r, method = c("medianImpute","center","scale"))
train_pp_r <- predict(preproc_r, train_dummy_r)
test_pp_r  <- predict(preproc_r, test_dummy_r)

# Separar variable respuesta
y_train <- train_raw$SalePrice
y_test  <- test_raw$SalePrice

# Modelo 1: nnet
ctrl_r1 <- trainControl(method = "cv", number = 10)
grid_r1 <- expand.grid(size = 5, decay = 0.5)
time_r1 <- system.time({
  model_r1 <- train(
    x = train_pp_r, y = y_train,
    method = "nnet",
    trControl = ctrl_r1,
    tuneGrid = grid_r1,
    linout = TRUE,
    trace = FALSE,
    MaxNWts = 5000,
    maxit = 200
  )
})

# Modelo 2: neuralnet
train_nn_r <- cbind(train_pp_r, SalePrice = y_train)
train_nn_r <- as.data.frame(train_nn_r)
names(train_nn_r) <- make.names(names(train_nn_r))

predictor_vars_r <- setdiff(names(train_nn_r), "SalePrice")
formula_r <- as.formula(paste("SalePrice ~", paste(predictor_vars_r, collapse = " + ")))
time_r2_adj <- system.time({
  model_r2_adj <- neuralnet(
    formula       = formula_r,
    data          = train_nn_r,
    hidden        = 3,           
    act.fct       = "logistic",  
    linear.output = TRUE,
    stepmax       = 1e6,
    threshold     = 0.005         
  )
})
# ------------------------------
# Paso 11: Comparar modelos de regresión
# ------------------------------

library(Metrics)

# --- Modelo 1: nnet ---
pred_r1 <- predict(model_r1, newdata = test_pp_r)

mae_r1  <- mae(y_test, pred_r1)
rmse_r1 <- rmse(y_test, pred_r1)
r2_r1   <- cor(y_test, pred_r1)^2  

# --- Modelo 2: neuralnet (3) --- 
raw_pred_r2 <- compute(model_r2_adj, test_pp_r)$net.result
pred_r2 <- as.vector(raw_pred_r2)

mae_r2  <- mae(y_test, pred_r2)
rmse_r2 <- rmse(y_test, pred_r2)
r2_r2   <- cor(y_test, pred_r2)^2

# --- Mostrar resultados ---
comparacion <- tibble(
  Modelo = c("nnet (5)", "neuralnet (3)"),
  MAE    = c(mae_r1, mae_r2),
  RMSE   = c(rmse_r1, rmse_r2),
  R2     = c(r2_r1, r2_r2)
)

print(comparacion)
# ------------------------------
# Paso 12: Sobreajuste
# ------------------------------

# Predicciones en entrenamiento (train)
train_pred_r1 <- predict(model_r1, newdata = train_pp_r)
train_pred_r2 <- predict(model_r2_adj, newdata = train_pp_r)

# Métricas de sobreajuste para modelo 1
mae_tr1  <- mae(y_train, train_pred_r1)
rmse_tr1 <- rmse(y_train, train_pred_r1)
r2_tr1   <- cor(y_train, train_pred_r1)^2

# Métricas de sobreajuste para modelo 2
mae_tr2  <- mae(y_train, train_pred_r2)
rmse_tr2 <- rmse(y_train, train_pred_r2)
r2_tr2   <- cor(y_train, train_pred_r2)^2

# Comparación entre métricas de entrenamiento y test
comparacion_sobreajuste <- tibble(
  Modelo    = c("nnet (5)", "neuralnet (3)"),
  MAE_Train = c(mae_tr1, mae_tr2),
  RMSE_Train= c(rmse_tr1, rmse_tr2),
  R2_Train  = c(r2_tr1, r2_tr2),
  MAE_Test  = c(mae_r1, mae_r2),
  RMSE_Test = c(rmse_r1, rmse_r2),
  R2_Test   = c(r2_r1, r2_r2)
)

# Mostrar resultados
print(comparacion_sobreajuste)
