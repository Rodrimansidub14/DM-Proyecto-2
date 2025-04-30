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
