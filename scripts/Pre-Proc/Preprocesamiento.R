
# 1. Cargar librerías necesarias
library(dplyr)
library(caret)
library(ggplot2)
library(gridExtra)

# Para funciones de agrupación de niveles infrecuentes
library(stringr)

# Para codificación ordinal
# Definimos el mapping de calidad: Ex > Gd > TA > Fa > Po
quality_mapping <- c("Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

# 2. Cargar el dataset
df <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)

# 3. Manejo de NAs -----------------------------------------

# Variables categóricas en las que NA indica ausencia de la característica
cat_vars_none <- c("Alley", "PoolQC", "MiscFeature", "Fence",
                   "FireplaceQu", "GarageType", "GarageFinish",
                   "GarageQual", "GarageCond", "BsmtQual", "BsmtCond",
                   "BsmtExposure", "BsmtFinType1", "BsmtFinType2")

for (var in cat_vars_none) {
  if(var %in% names(df)) {
    df[[var]][is.na(df[[var]])] <- "None"
  }
}

# Variables de área donde la ausencia se interpreta como 0
area_vars <- c("LotFrontage", "GarageArea", "TotalBsmtSF")
for (var in area_vars) {
  if(var %in% names(df)) {
    df[[var]][is.na(df[[var]])] <- 0
  }
}

# Eliminar variables con más del 40% de NAs
na_threshold <- 0.4
na_pct <- sapply(df, function(x) mean(is.na(x)))
vars_to_drop <- names(na_pct[na_pct > na_threshold])
if(length(vars_to_drop) > 0) {
  df <- df %>% select(-one_of(vars_to_drop))
}

# 4. Agrupación de categorías poco frecuentes ------------------

# Función para agrupar niveles con frecuencia menor a un umbral
group_infrequent <- function(x, threshold = 0.05) {
  freq <- table(x) / length(x)
  levels_to_keep <- names(freq[freq >= threshold])
  x[!(x %in% levels_to_keep)] <- "Other"
  return(factor(x))
}

# Variables nominales para agrupar (ejemplos comunes)
nominal_vars <- c("Neighborhood", "BldgType", "HouseStyle", "Exterior1st", "Exterior2nd")
for (var in nominal_vars) {
  if(var %in% names(df)) {
    df[[var]] <- group_infrequent(df[[var]], threshold = 0.05)
  }
}

# 5. Codificación ---------------------------------------------

# 5.1 One-Hot encoding para variables nominales
# Usamos dummyVars de caret
dummy_vars <- nominal_vars  # Utilizamos las mismas variables nominales definidas
if(length(dummy_vars) > 0) {
  dummies <- dummyVars(~ ., data = df[, dummy_vars], fullRank = TRUE)
  df_dummies <- predict(dummies, newdata = df[, dummy_vars])
  df <- cbind(df, df_dummies)
  df <- df %>% select(-one_of(dummy_vars))
}

# 5.2 Ordinal encoding para variables de calidad
ordinal_vars <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
                  "GarageQual", "GarageCond", "HeatingQC", "KitchenQual")
for (var in ordinal_vars) {
  if(var %in% names(df)) {
    # Convertir a factor si no lo es y luego mapear
    df[[var]] <- as.character(df[[var]])
    df[[var]] <- as.numeric(quality_mapping[df[[var]]])
  }
}

# 6. Outliers y transformación -------------------------------

# Función para capping: recortar valores por encima del percentil 99
cap_outliers <- function(x, cap = 0.99) {
  quant_val <- quantile(x, probs = cap, na.rm = TRUE)
  x[x > quant_val] <- quant_val
  return(x)
}

# Variables numéricas a evaluar
numeric_vars <- c("SalePrice", "GrLivArea", "LotArea", "TotalBsmtSF", "GarageArea", "MasVnrArea")
for (var in numeric_vars) {
  if(var %in% names(df)) {
    df[[var]] <- cap_outliers(df[[var]], cap = 0.99)
  }
}

# Transformar variables para reducir skew
# Nota: Se añade 1 a GrLivArea y LotArea para evitar log(0)
df$SalePrice <- log(df$SalePrice)
df$GrLivArea <- log(df$GrLivArea + 1)
df$LotArea   <- log(df$LotArea + 1)

# 7. Feature Engineering -------------------------------------

# Crear TotalArea: suma de GrLivArea, TotalBsmtSF y GarageArea (si existen)
if(all(c("GrLivArea", "TotalBsmtSF", "GarageArea") %in% names(df))) {
  df$TotalArea <- df$GrLivArea + df$TotalBsmtSF + df$GarageArea
}

# Crear HouseAge: Año de venta menos Año de construcción (si existen)
if(all(c("YrSold", "YearBuilt") %in% names(df))) {
  df$HouseAge <- df$YrSold - df$YearBuilt
}

# Crear TotalBath: Sumar baños completos y la mitad de los semibaños
if(all(c("FullBath", "HalfBath", "BsmtFullBath", "BsmtHalfBath") %in% names(df))) {
  df$TotalBath <- df$FullBath + 0.5 * df$HalfBath + df$BsmtFullBath + 0.5 * df$BsmtHalfBath
}

# Verificar correlación con SalePrice (opcional)
num_cols <- names(df)[sapply(df, is.numeric)]
correlations <- cor(df[, num_cols], use = "pairwise.complete.obs")
print(correlations["SalePrice", ])

# 8. Escalado ------------------------------------------------

# Estandarizar variables numéricas
df[num_cols] <- lapply(df[num_cols], scale)

# 9. Validación: Separación en train y test ---------------

set.seed(123)
train_index <- createDataPartition(df$SalePrice, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

# 10. Guardar conjuntos preprocesados ---------------------
write.csv(train_data, "data/processed/train_preprocessed.csv", row.names = FALSE)
write.csv(test_data, "data/processed/test_preprocessed.csv", row.names = FALSE)


