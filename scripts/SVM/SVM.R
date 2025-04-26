# --------------------------------------------------
# SVM – Puntos 1–3 (pipeline corregido v3)
# --------------------------------------------------

# 1. Cargar librerías y datos
# --------------------------------------------------------------------
library(tidyverse)    #  dplyr, readr, etc.
library(caret)        # createDataPartition, dummyVars, preProcess
library(e1071)        # svm

set.seed(123)

train_raw <- read_csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv")
test_raw  <- read_csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/test_preprocessed.csv")


# 2. Explorar y transformar datos
# -----------------------------------------------------

# 2.1 Ver NA por columna
na_counts <- colSums(is.na(train_raw))
print(na_counts[na_counts > 0])

# 2.2 Crear PriceCat en base a cuartiles 25% y 75% de SalePrice
cuartiles       <- quantile(train_raw$SalePrice, probs = c(0.25, 0.75))
lower_threshold <- cuartiles[1]
upper_threshold <- cuartiles[2]

train_raw <- train_raw %>%
  mutate(PriceCat = case_when(
    SalePrice < lower_threshold ~ "Economicas",
    SalePrice < upper_threshold ~ "Intermedias",
    TRUE                         ~ "Caras"
  )) %>%
  mutate(PriceCat = factor(PriceCat, 
                           levels = c("Economicas","Intermedias","Caras")))

test_raw <- test_raw %>%
  mutate(PriceCat = case_when(
    SalePrice < lower_threshold ~ "Economicas",
    SalePrice < upper_threshold ~ "Intermedias",
    TRUE                         ~ "Caras"
  )) %>%
  mutate(PriceCat = factor(PriceCat, 
                           levels = c("Economicas","Intermedias","Caras")))

# 2.3 Preparar datos para SVM
#    • Quitar Id y SalePrice
#    • Transformar chr→factor
#    • Alinear niveles de factores en test a los de train
#    • One-hot encode
# -----------------------------------------------------
drop_vars <- c("Id","SalePrice")
train_mod <- train_raw %>% select(-all_of(drop_vars))
test_mod  <- test_raw  %>% select(-all_of(drop_vars))

# chr → factor
train_mod <- train_mod %>% mutate(across(where(is.character), factor))
test_mod  <- test_mod  %>% mutate(across(where(is.character), factor))

# Alinear niveles de cada factor en test_mod a los de train_mod
fac_vars <- names(train_mod)[sapply(train_mod, is.factor)]
for(var in fac_vars) {
  test_mod[[var]] <- factor(test_mod[[var]],
                            levels = levels(train_mod[[var]]))
}

# Eliminar factores que en train_mod tengan un solo nivel
onelev <- fac_vars[sapply(train_mod[fac_vars], nlevels)==1]
if(length(onelev)>0){
  train_mod <- train_mod %>% select(-all_of(onelev))
  test_mod  <- test_mod  %>% select(-all_of(onelev))
}

# One-hot encoding (excluye PriceCat)
dv <- dummyVars(~ . - PriceCat, data = train_mod, fullRank = TRUE)
train_dum <- predict(dv, newdata = train_mod) %>% as.data.frame()
test_dum  <- predict(dv, newdata = test_mod)  %>% as.data.frame()

# volver a añadir PriceCat
train_pp <- train_dum %>% mutate(PriceCat = train_mod$PriceCat)
test_pp  <- test_dum  %>% mutate(PriceCat = test_mod$PriceCat)

# 2.4 Imputar y escalar
#    • medianImpute, center, scale
# -----------------------------------------------------
preProcValues <- preProcess(train_pp %>% select(-PriceCat),
                            method = c("medianImpute","center","scale"))

train_scaled <- predict(preProcValues, train_pp %>% select(-PriceCat))
test_scaled  <- predict(preProcValues, test_pp  %>% select(-PriceCat))

train_final <- train_scaled %>% mutate(PriceCat = train_pp$PriceCat)
test_final  <- test_scaled  %>% mutate(PriceCat = test_pp$PriceCat)


# 3. Verificar Puntos 1–3
# -----------------------------------------------------
cat("Dimensiones train_final:", dim(train_final), "\n")
cat("Dimensiones test_final: ", dim(test_final),  "\n")
cat("Train PriceCat distribuciones:\n"); print(table(train_final$PriceCat))
cat("Test  PriceCat distribuciones:\n"); print(table(test_final$PriceCat))

# --------------------------------------------------
# 4. Entrenar múltiples SVM con distintos kernels e hiperparámetros
# --------------------------------------------------

# Definir fórmulas y datos
formula_svm <- PriceCat ~ .

# (a) SVM lineal con diferentes valores de cost
costs_lin <- c(0.1, 1, 10)
svm_lin_models <- lapply(costs_lin, function(C) {
  svm(formula_svm, data = train_final,
      kernel = "linear",
      cost   = C,
      scale  = FALSE)
})
names(svm_lin_models) <- paste0("svm_lin_C", costs_lin)

# (b) SVM radial con grillas de cost y gamma
costs_rad  <- c(1, 10)
gammas_rad <- c(0.001, 0.01, 0.1)
svm_rad_models <- list()
for (C in costs_rad) {
  for (g in gammas_rad) {
    nm <- paste0("svm_rad_C", C, "_g", g)
    svm_rad_models[[nm]] <- svm(formula_svm, data = train_final,
                                kernel = "radial",
                                cost   = C,
                                gamma  = g,
                                scale  = FALSE)
  }
}

# (c) SVM polinomial con distintas combinaciones de cost, gamma y degree
degrees    <- c(2, 3)
costs_poly <- c(1, 5)
gammas_poly <- c(0.01, 0.1)
svm_poly_models <- list()
for (d in degrees) {
  for (C in costs_poly) {
    for (g in gammas_poly) {
      nm <- paste0("svm_poly_deg", d, "_C", C, "_g", g)
      svm_poly_models[[nm]] <- svm(formula_svm, data = train_final,
                                   kernel = "polynomial",
                                   degree = d,
                                   cost   = C,
                                   gamma  = g,
                                   scale  = FALSE)
    }
  }
}

# (d) Búsqueda automática (tuning) para kernel radial
set.seed(123)
tune_radial <- tune.svm(formula_svm, data = train_final,
                        kernel = "radial",
                        cost  = 2^(-1:2),
                        gamma = 2^(-3:-1))
print(tune_radial)                # muestra performance para cada combinación
best_radial <- tune_radial$best.model
cat("Mejor radial: cost =", best_radial$cost,
    " gamma =", best_radial$gamma, "\n\n")


# --------------------------------------------------
# 5. Predicción de la variable respuesta en test_final
# --------------------------------------------------
predict_svm <- function(model_list, data) {
  lapply(model_list, function(m) {
    predict(m, newdata = data)
  })
}

pred_lin  <- predict_svm(svm_lin_models, test_final)
pred_rad  <- predict_svm(svm_rad_models,  test_final)
pred_poly <- predict_svm(svm_poly_models, test_final)
pred_best <- predict(best_radial,          newdata = test_final)


# --------------------------------------------------
# 6. Matrices de confusión
# --------------------------------------------------

# (a) Lineales
for (nm in names(pred_lin)) {
  cat("=== SVM lineal –", nm, "===\n")
  print(confusionMatrix(pred_lin[[nm]], test_final$PriceCat))
  cat("\n")
}

# (b) Radiales
for (nm in names(pred_rad)) {
  cat("=== SVM radial –", nm, "===\n")
  print(confusionMatrix(pred_rad[[nm]], test_final$PriceCat))
  cat("\n")
}

# (c) Polinomiales
for (nm in names(pred_poly)) {
  cat("=== SVM polinomial –", nm, "===\n")
  print(confusionMatrix(pred_poly[[nm]], test_final$PriceCat))
  cat("\n")
}

# (d) Mejor radial tuneado
cat("=== SVM radial tuneado (best_radial) ===\n")
print(confusionMatrix(pred_best, test_final$PriceCat))


# 7. Visualización de fronteras y distribuciones
# 7. Visualización de fronteras 2D (only 2 features)
# --------------------------------------------------

# 7.1 Extraer un data.frame con sólo dos predictoras + PriceCat
df_vis <- train_final %>% 
  select(OverallQual, GrLivArea, PriceCat)

# 7.2 Entrenar un SVM 2D con los mismos hiperparámetros "best_radial"
svm_vis2 <- svm(PriceCat ~ OverallQual + GrLivArea,
                data   = df_vis,
                kernel = "radial",
                cost   = best_radial$cost,
                gamma  = best_radial$gamma,
                scale  = FALSE)

# 7.3 Frontera con plot.svm
plot(svm_vis2, df_vis, OverallQual ~ GrLivArea,
     main = "Frontera SVM radial\nOverallQual vs GrLivArea")

# 7.4 Scatter con ggplot2
library(ggplot2)
ggplot(df_vis, aes(OverallQual, GrLivArea, color = PriceCat)) +
  geom_point(alpha = 0.6) +
  labs(title = "Distribución 2D por PriceCat",
       x = "OverallQual", y = "GrLivArea") +
  theme_minimal()

# 7.5 Frontera con geom_contour
grid2 <- expand.grid(
  OverallQual = seq(min(df_vis$OverallQual), max(df_vis$OverallQual), length = 100),
  GrLivArea   = seq(min(df_vis$GrLivArea),   max(df_vis$GrLivArea),   length = 100)
)
grid2$Pred <- predict(svm_vis2, newdata = grid2)

ggplot() +
  geom_point(data = df_vis, aes(OverallQual, GrLivArea, color = PriceCat), alpha = 0.5) +
  geom_contour(data = grid2, 
               aes(x = OverallQual, y = GrLivArea, z = as.numeric(Pred)),
               bins = 2, color = "black") +
  labs(title = "Frontera SVM radial con geom_contour") +
  theme_minimal()


# 8. Gráfica 3D interactiva con plotly
# -------------------------------------
library(plotly)

# Prepara el data frame con tres variables
df3d <- train_final %>% 
  select(OverallQual, GrLivArea, YearBuilt, PriceCat)

# Crea la gráfica 3D
fig <- plot_ly(df3d,
               x = ~OverallQual, 
               y = ~GrLivArea, 
               z = ~YearBuilt,
               color = ~PriceCat,
               colors = c("blue","orange","darkred"),
               marker = list(size = 3)) %>%
  add_markers() %>%
  layout(title = "Distribución 3D de PriceCat",
         scene = list(xaxis = list(title = "OverallQual"),
                      yaxis = list(title = "GrLivArea"),
                      zaxis = list(title = "YearBuilt")))

fig


# 7. Análisis de sobreajuste y desajuste
# --------------------------------------

# 7. Análisis de sobreajuste/desajuste – corrección de lista de modelos
# ---------------------------------------------------------------------

# a) Reunir correctamente todos los modelos en una lista
svm_all <- c(
  svm_lin_models,
  svm_rad_models,
  svm_poly_models,
  list(best_radial = best_radial)
)

# b) Calcular accuracy en train y test
library(caret)
eval_df <- lapply(names(svm_all), function(nm) {
  m   <- svm_all[[nm]]
  trp <- predict(m, train_final)
  tep <- predict(m, test_final)
  tr_acc <- confusionMatrix(trp, train_final$PriceCat)$overall["Accuracy"]
  te_acc <- confusionMatrix(tep, test_final$PriceCat)$overall["Accuracy"]
  data.frame(Model = nm,
             TrainAcc = as.numeric(tr_acc),
             TestAcc  = as.numeric(te_acc),
             Diff     = as.numeric(tr_acc - te_acc))
}) %>% bind_rows()

print(eval_df)

# c) Interpretación breve
#  - Si TrainAcc mucho mayor que TestAcc → sobreajuste.
#    • Reducir cost o gamma, usar menos features, aplicar PCA o regularización.
#    • Aumentar más datos o usar validación cruzada anidada.
#  - Si ambos accuracies bajos → desajuste.
#    • Incrementar complejidad (mayor cost/gamma, probar kernels polinomiales de grado mayor).
#    • Crear nuevas variables o enriquecer el set de features.
#  - Si Diff ≈ 0 y TestAcc alto → buen ajuste, el modelo generaliza bien.

