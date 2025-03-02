#############################################
# Análisis de Regresión Lineal Múltiple
# Proyecto: Predicción del Precio de Casas
#############################################

# 1. Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(caret)      # Para partición y evaluación
library(car)        # Para calcular VIF y detectar multicolinealidad
library(nortest)    # Para pruebas de normalidad de residuos
library(glmnet)     # Para Ridge y Lasso
library(tidyr)      # Para reorganizar datos (si es necesario)
library(GGally)     # Para matriz de dispersión
library(corrplot)
library(tibble)


# 2. Cargar el dataset preprocesado
# Se asume que "train.csv" ya está preprocesado (manejo de NAs, transformaciones, etc.)
datostrain <- read.csv("data/processed/train_preprocessed.csv", stringsAsFactors = FALSE)
datostest <- read.csv("data/processed/test_preprocessed.csv", stringsAsFactors = FALSE)

cat("Número de filas en entrenamiento:", nrow(datostrain), "\n")
cat("Número de filas en prueba:", nrow(datostest), "\n")


#############################################
#  Modelo Univariado
#############################################
# 1.2. Modelo univariado:
modelo_uni <- lm(SalePrice ~ GrLivArea, data = datostrain)
summary(modelo_uni)
# Representación gráfica del modelo univariado

# Gráfica del modelo univariado
ggplot(datostrain, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Regresión Lineal Univariada: SalePrice ~ GrLivArea",
       x = "GrLivArea", y = "SalePrice`") +
  theme_bw()

# Análisis de residuos

predL<-predict(modelo_uni, newdata = datostest)

head(predL)
length(predL)


head(modelo_uni$residuals)

par(mfrow = c(2, 2))
plot(modelo_uni)
par(mfrow = c(1, 1))
# Histogramas, boxplot y QQ-plot de los residuos

hist(modelo_uni$residuals, breaks = 30, col = "lightblue", border = "black", main = "Histograma de Residuos", xlab = "Residuos")

boxplot(modelo_uni$residuals, col = "lightblue", border = "black", main = "Boxplot de Residuos")

qqnorm(modelo_uni$residuals, col = "blue", pch = 20, main = "Gráfico Q-Q de Residuos")
qqline(modelo_uni$residuals, col = "red")
# Prueba de normalidad (Lilliefors)

lillie.test(modelo_uni$residuals)


# 1.4. Predicción con el modelo univariado
pred_uni <- predict(modelo_uni, newdata = datostest)
head(pred_uni)
rmse_uni <- RMSE(pred_uni, datostest$SalePrice)
cat("RMSE del modelo univariado:", rmse_uni, "\n")

#Grafico de la predicción
plot(datostest$SalePrice, col = "blue")
points(pred_uni, col = "red")

summary(datostest$SalePrice-pred_uni)


#############################################
# 6. Modelo Múltiple con Todas las Variables Seleccionadas
#############################################
# 1. Estandarizar los nombres de columnas en ambos datasets
names(train_data) <- make.names(names(train_data))
names(test_data)  <- make.names(names(test_data))

# 2. Seleccionar todas las variables numéricas en cada conjunto
num_vars_train <- names(train_data)[sapply(train_data, is.numeric)]
num_vars_test  <- names(test_data)[sapply(test_data, is.numeric)]

# 3. Identificar las variables numéricas comunes a ambos datasets
common_vars <- intersect(num_vars_train, num_vars_test)
cat("Variables numéricas comunes:\n")
print(common_vars)
# Verificar que la variable respuesta "SalePrice" está presente
if(!"SalePrice" %in% common_vars) stop("La variable 'SalePrice' no se encuentra en los datos numéricos comunes.")

vars_seleccionadas <-  c("SalePrice",   # Variable objetivo
                         "GrLivArea", 
                         "LotArea",
                         "OverallQual",
                         "YearBuilt",
                         "TotalBsmtSF",
                         "GarageArea")
pairs(train_data[, vars_seleccionadas], main = "Matriz de Dispersión Reducida")


matriz_corrpairs <- cor(train_data[, vars_seleccionadas])
matriz_corrpairs

corrplot(matriz_corrpairs)

# 2. Definir los predictores (todas las variables numéricas excepto la respuesta)
predictors <- setdiff(common_vars, "SalePrice")
cat("Predictores seleccionados:\n")
print(predictors)

# 6. Filtrar filas completas (sin NAs) para las variables de interés en ambos conjuntos
train_filtered <- train_data[, c("SalePrice", predictors)]
train_filtered <- train_filtered[complete.cases(train_filtered), ]
test_filtered  <- test_data[, c("SalePrice", predictors)]
test_filtered  <- test_filtered[complete.cases(test_filtered), ]
# 3. Crear la fórmula del modelo
print(formula_numeric)
# (Opcional) Verificar dimensiones de los conjuntos filtrados
cat("Dimensiones de train_filtered:", dim(train_filtered), "\n")
cat("Dimensiones de test_filtered:", dim(test_filtered), "\n")

model_multi <- lm(SalePrice ~ ., data = train_filtered)
summary(model_multi)
# 5. Análisis gráfico de residuos
par(mfrow = c(2, 2))
plot(model_multi)
par(mfrow = c(1, 1))

hist(model_multi$residuals, breaks = 30, col = "lightblue", border = "black", main = "Histograma de Residuos", xlab = "Residuos")
boxplot(model_multi$residuals, col = "lightblue", border = "black", main = "Boxplot de Residuos")



# 6. Prueba de normalidad de los residuos (Lilliefors)
lillie_result <- lillie.test(model_multi$residuals)
print(lillie_result)

pred_test <- predict(model_multi, newdata = test_filtered)
rmse_multi <- RMSE(pred_test, test_filtered$SalePrice)
cat("RMSE del modelo multivariado en test:", rmse_multi, "\n")


plot(test_filtered$SalePrice, col = "blue")
points(pred_test, col = "red")


summary(test_filtered$SalePrice-pred_test)


#Analizando errores de entrenamiento y prueba
prediccion_train <- predict(model_multi, newdata = train_filtered)
prediccion_test <- predict(model_multi, newdata = test_filtered)

train_mse <- mean((prediccion_train - train_filtered$SalePrice)^2)
test_mse <- mean((prediccion_test - test_filtered$SalePrice)^2)


cat("MSE en entrenamiento:", train_mse, "\n")
cat("MSE en prueba:", test_mse, "\n")


## Normalizando los datos

train_filtered_norm <- as.data.frame(scale(train_filtered))
test_filtered_norm <- as.data.frame(scale(test_filtered))

model_multi_norm <- lm(SalePrice ~ ., data = train_filtered_norm)
summary(model_multi_norm)
par(mfrow = c(2, 2))
plot(model_multi_norm)
par(mfrow = c(1, 1))

## Seleccionando predictores

modelo_mult2  <- step(
 object = lm(formula = SalePrice ~ ., data = train_filtered),
 direction = "backward",
 scope = list(upper = ~ ., lower = ~ 1),
 trace = FALSE
)

summary(modelo_mult2)

par(mfrow = c(2, 2))

par(mfrow = c(1, 1))


lillie.test(modelo_mult2$residuals)

prediccion_train2 <- predict(modelo_mult2, newdata = train_filtered)
prediccion_test2 <- predict(modelo_mult2, newdata = test_filtered)

train_stepwise <- mean((prediccion_train2 - train_filtered$SalePrice)^2)
test_stepwise_mse <- mean((prediccion_test2 - test_filtered$SalePrice)^2)


cat("MSE en entrenamiento:", train_stepwise, "\n")
cat("MSE en prueba:", test_stepwise_mse, "\n")


# 7. Regularización con Ridge y Lasso
x_train <- model.matrix(SalePrice ~ ., data = train_filtered)[, -1]
y_train <- train_filtered$SalePrice

x_test <- model.matrix(SalePrice ~ ., data = test_filtered)[, -1]
y_test <- test_filtered$SalePrice

#regularicaxcion con 100 valores de lambda
model_ridge <- glmnet(x = x_train, y = y_train, alpha = 0, nlambda = 100, standardize = TRUE)
regularization <- model_ridge$beta %>% as.matrix() %>% t() %>% as.tibble() %>% mutate(lambda = model_ridge$lambda)
regularization  <- regularization %>% pivot_longer(cols = -lambda, names_to = "predictor", values_to = "coefficients")


regularization %>% 
  ggplot(aes(x = lambda, y = coefficients, color = predictor)) +
  geom_line() +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
print(cv_error)
plot(cv_error)


model3 <- glmnet(x = x_train, y = y_train, alpha = 0, lambda = cv_error$lambda.1se, standardize = TRUE)



coef(model3)

pred_train_model3 <- predict(model3,newx = x_train)

pred_test_model3 <- predict(model3,newx = x_test)


test_mse_model3 <- mean((pred_test_model3 - y_test)^2)
cat("MSE en prueba:", test_mse_model3, "\n")

cat("MSE en entrenamiento:", mean((pred_train_model3 - y_train)^2), "\n")



## Lasso


model_lasso <- glmnet(x = x_train, y = y_train, alpha = 1, nlambda = 100, standardize = TRUE)

regularization_lasso <- model_lasso$beta %>% as.matrix() %>% t() %>% as.tibble() %>% mutate(lambda = model_lasso$lambda)

regularization_lasso  <- regularization_lasso %>% pivot_longer(cols = !lambda, names_to = "predictor", values_to = "coefficients")


regularization_lasso %>% 
  ggplot(aes(x = lambda, y = coefficients, color = predictor)) +
  geom_line() +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_lasso)


model_lasso <- glmnet(x = x_train, y = y_train, alpha = 1, lambda = cv_error_lasso$lambda.1se, standardize = TRUE)


coef(model_lasso)


pred_train_model_lasso <- predict(model_lasso,newx = x_train)

pred_test_model_lasso <- predict(model_lasso,newx = x_test)


test_mse_model_lasso <- mean((pred_test_model_lasso - y_test)^2)

train_mse_model_lasso <- mean((pred_train_model_lasso - y_train)^2)

cat("MSE en prueba:", test_mse_model_lasso, "\n")

cat("MSE en entrenamiento:", train_mse_model_lasso, "\n")



#Comparacion de Modelos


modelos <- c( "Multivariado", "Multivariado Normalizado", "Multivariado Stepwise", "Ridge", "Lasso")
mse <- c( test_mse, test_mse, test_stepwise_mse, test_mse_model3, test_mse_model_lasso)
ggplot(data = data.frame(modelos, mse), aes(x = modelos, y = mse)) +
  geom_col(fill = "lightblue", color = "black") +
  geom_text(aes(label = round(mse, 2)), vjust = -0.5) +
  labs(title = "Comparación de Modelos", x = "Modelo", y = "MSE") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#AIC y BIC
##################################################
# 1. Definir la función para AICc y BIC en glmnet
##################################################
glmnet_cv_aicc <- function(fit) {
  # 'fit' es el objeto glmnet ajustado (por ejemplo, model3 o model_lasso)
  # tLL: diferencia entre la nula y la deviance actual
  tLL <- fit$nulldev - deviance(fit)
  # k: número de predictores en cada lambda (vector)
  k <- fit$df
  # n: número de observaciones
  n <- fit$nobs
  
  # AICc para cada lambda
  AICc <- -tLL + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
  
  # BIC para cada lambda
  BIC <- log(n) * k - tLL
  
  # Retornar como lista
  list(AICc = AICc, BIC = BIC)
}

##################################################
# 2. Calcular AIC y BIC para el modelo lineal
##################################################
aic_linear <- AIC(model_multi)
bic_linear <- BIC(model_multi)

#aic stepwise

aic_stepwise <- AIC(modelo_mult2)
bic_stepwise <- BIC(modelo_mult2)


##################################################
# 3. Calcular AICc y BIC para Ridge y Lasso
##################################################
# Nota: glmnet produce un vector de lambdas. 
# La función glmnet_cv_aicc() devuelve un vector de AICc y BIC 
# para cada lambda en 'fit$lambda'.

aic_bic_ridge <- glmnet_cv_aicc(model3)     # model3 es tu modelo Ridge
aic_bic_lasso <- glmnet_cv_aicc(model_lasso) # model_lasso es tu modelo Lasso

# Si deseas elegir un lambda en particular (por ejemplo, el lambda.1se),
# convendría extraer la posición del lambda en 'model3$lambda' y tomar
# la AICc/BIC correspondiente.

# Por ejemplo, si ya usaste 'model3 <- glmnet(..., lambda = cv_error$lambda.1se)',
# 'model3$lambda' es un solo valor. 
# Si, en cambio, usaste nlambda = 100, tendrás 100 lambdas.

# Supongamos que en 'model3' solo hay un lambda. 
# Entonces aic_bic_ridge$AICc y aic_bic_ridge$BIC serán vectores de longitud 1 (o más).
# Toma el primer valor si es uno solo:
ridge_aicc <- aic_bic_ridge$AICc[1]
ridge_bic  <- aic_bic_ridge$BIC[1]

lasso_aicc <- aic_bic_lasso$AICc[1]
lasso_bic  <- aic_bic_lasso$BIC[1]

##################################################
# 4. Crear un data frame con la comparación
##################################################
dfMetricas <- data.frame(
  Modelo = c("Lineal Múltiple", "Stepwise", "Ridge", "Lasso"),
  AIC_or_AICc = c(aic_linear, aic_stepwise, ridge_aicc, lasso_aicc),
  BIC = c(bic_linear, bic_stepwise, ridge_bic, lasso_bic)
)


dfMetricas


plot(test_filtered$SalePrice, col = "blue",main = "Predicciones vs valores reales")
points(pred_test, col = "red")
legend("topright", legend = c("Real", "Predicción"), col = c("blue", "red"), pch = 1)









