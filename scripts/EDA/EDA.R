# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)
library(patchwork)



# Cargar el dataset (ajusta la ruta si es necesario)
datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)

# 1. Revisión de la Estructura y Estadísticas Básicas
cat("Número de filas:", nrow(datos), "\n")
cat("Número de columnas:", ncol(datos), "\n")


str(datos)

cat("\nPrimeras 3 filas:\n")
print(head(datos, 6))

cat("\nÚltimas 3 filas:\n")
print(tail(datos, 6))

cat("\nVisualización de las primeras 5 columnas:\n")
print(head(datos[, 1:5]))


# 2. Análisis descriptivo

library(knitr)

# Seleccionar las columnas numéricas del dataset 'datos'
numeric_cols <- datos[sapply(datos, is.numeric)]

# Función para calcular las estadísticas descriptivas
calc_stats <- function(x) {
  c(
    count = sum(!is.na(x)),
    mean  = mean(x, na.rm = TRUE),
    std   = sd(x, na.rm = TRUE),
    min   = min(x, na.rm = TRUE),
    "25%" = quantile(x, 0.25, na.rm = TRUE),
    "50%" = quantile(x, 0.50, na.rm = TRUE),  # Este valor es la mediana
    "75%" = quantile(x, 0.75, na.rm = TRUE),
    max   = max(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE)
  )
}

# Aplicar la función a cada columna numérica y transponer el resultado
resumen_estadistico <- t(sapply(numeric_cols, calc_stats))
resumen_estadistico <- as.data.frame(resumen_estadistico)

# Redondear los valores para mejorar la legibilidad
resumen_estadistico[] <- lapply(resumen_estadistico, function(x) round(x, 2))

# Mostrar el resumen estadístico con kable
cat("Resumen estadístico de variables numéricas:\n\n")
kable(resumen_estadistico, caption = "Resumen Estadístico de Variables Numéricas")



# Variables categóricas



# Seleccionar las variables categóricas: asumimos que son de tipo character o factor
vars_categoricas <- names(datos)[sapply(datos, function(x) is.character(x) || is.factor(x))]

# Imprimir un mensaje para indicar el inicio del análisis general de variables categóricas
cat("Análisis general de variables categóricas\n")
cat("=========================================\n\n")

# Loop para cada variable categórica
for (var in vars_categoricas) {
  cat("Analizando variable:", var, "\n")
  
  # Generar tabla de frecuencias
  freq_table <- table(datos[[var]])
  print(freq_table)
  
  # Gráfico de barras de la distribución de frecuencias
  p1 <- ggplot(datos, aes_string(x = var)) +
    geom_bar(fill = "steelblue") +
    labs(title = paste("Frecuencia de", var),
         x = var,
         y = "Conteo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p1)
  
  # Si existe la variable objetivo SalePrice, generar boxplot para analizar la relación
  if ("SalePrice" %in% names(datos)) {
    p2 <- ggplot(datos, aes_string(x = var, y = "SalePrice")) +
      geom_boxplot(outlier.colour = "red", fill = "lightblue") +
      labs(title = paste("Distribución de SalePrice por", var),
           x = var,
           y = "SalePrice") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p2)
  }
  
  cat("\n-----------------------------------------\n\n")
}

# Ejemplo de análisis de relaciones cruzadas entre dos variables categóricas:
# Aquí se usa "Neighborhood" y "HouseStyle" como ejemplo. Asegúrate de que existan en tu dataset.
if (all(c("Neighborhood", "HouseStyle") %in% names(datos))) {
  cat("Tabla de contingencia entre Neighborhood y HouseStyle:\n")
  tabla_cruzada <- table(datos$Neighborhood, datos$HouseStyle)
  print(tabla_cruzada)
  
  # Generar un mosaic plot para visualizar la relación
  mosaicplot(tabla_cruzada, main = "Mosaic Plot: Neighborhood vs HouseStyle",
             color = TRUE, las = 2)
}
# ------------------------------------------------------
# SCRIPT DE IDENTIFICACIÓN DE PROBLEMAS EN EL DATASET
# ------------------------------------------------------

# Cargar librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Leer el dataset (ajusta la ruta según corresponda)
datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)

# 1. IDENTIFICACIÓN DE VALORES FALTANTES Y ERRORES DE CODIFICACIÓN
cat("### Análisis de Valores Faltantes y Errores de Codificación ###\n\n")

# Variables de interés: LotFrontage, Alley, PoolQC, Fence, MiscFeature
vars_missing <- c("LotFrontage", "Alley", "PoolQC", "Fence", "MiscFeature")

for (var in vars_missing) {
  cat("Variable:", var, "\n")
  
  # Calcular el número y porcentaje de valores faltantes
  missing_count <- sum(is.na(datos[[var]]))
  missing_percent <- (missing_count / nrow(datos)) * 100
  cat("  Valores faltantes:", missing_count, "(", round(missing_percent, 2), "% )\n")
  
  # Mostrar los valores únicos para detectar posibles errores o codificaciones extrañas
  unique_vals <- unique(datos[[var]])
  cat("  Valores únicos:", paste(unique_vals, collapse = ", "), "\n\n")
}

# 2. IDENTIFICACIÓN DE OUTLIERS Y NECESIDAD DE TRANSFORMACIÓN
cat("### Análisis de Outliers y Posibles Transformaciones ###\n\n")

# Variables de interés para evaluar outliers y transformaciones
vars_outlier <- c("SalePrice", "GrLivArea", "LotArea", "X1stFlrSF", "TotalBsmtSF", "MasVnrArea", "GarageArea")

for (var in vars_outlier) {
  cat("Variable:", var, "\n")
  
  # Mostrar resumen estadístico básico
  print(summary(datos[[var]]))
  
  # Calcular cuantiles importantes
  quantiles <- quantile(datos[[var]], probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99), na.rm = TRUE)
  cat("  Cuantiles (1%, 5%, 25%, 50%, 75%, 95%, 99%):\n")
  print(quantiles)
  
  # Graficar histograma para visualizar la distribución
  p_hist <- ggplot(datos, aes_string(x = var)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
    theme_minimal()
  print(p_hist)
  
  # Graficar boxplot para detectar outliers
  p_box <- ggplot(datos, aes_string(y = var)) +
    geom_boxplot(fill = "orange", outlier.colour = "red", outlier.shape = 16) +
    labs(title = paste("Boxplot de", var), y = var) +
    theme_minimal()
  print(p_box)
  
  cat("\n-----------------------------------------\n\n")
  

  # -------------------------------
  # Pruebas de Normalidad
  # -------------------------------
  
  cat("### Pruebas de Normalidad ###\n\n")
  
  # Seleccionar variables clave para el test
  variables_clave <- c("SalePrice", "GrLivArea", "LotArea", "X1stFlrSF", "TotalBsmtSF")
  
  # Ejecutar la prueba de Shapiro-Wilk para cada variable
  normality_results <- list()
  for (var in variables_clave) {
    # Asegurarse que la variable no contenga NA y tenga suficientes observaciones
    datos_var <- na.omit(datos[[var]])
    if (length(datos_var) >= 3) {  # Shapiro requiere al menos 3 observaciones
      test <- shapiro.test(datos_var)
      normality_results[[var]] <- test
      cat("Prueba de Shapiro-Wilk para", var, ":\n")
      print(test)
      cat("\n")
    } else {
      cat("No hay suficientes datos para realizar la prueba en", var, "\n\n")
    }
  }
  
  # Nota: Si el p-valor es menor a 0.05, se rechaza la hipótesis de normalidad, indicando la posible necesidad de transformar la variable.
  
  # Cargar librería nortest (si no está instalada, descomenta la siguiente línea)
  # install.packages("nortest")
  library(nortest)
  
  # 1. Cargar librerías necesarias
  if (!require(nortest)) {
    install.packages("nortest")
    library(nortest)
  } else {
    library(nortest)
  }
  
  # 2. Cargar el dataset (ajusta la ruta si es necesario)
  datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)
  
  # 3. Definir los grupos de variables a evaluar
  
  # Grupo 1: Variable objetivo y áreas
  vars_group1 <- c("SalePrice", "GrLivArea", "LotArea", "X1stFlrSF", "TotalBsmtSF")
  
  # Grupo 2: Variables de calidad y construcción
  vars_group2 <- c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd")
  
  # Grupo 3: Variables relacionadas con acabados y garaje
  vars_group3 <- c("MasVnrArea", "GarageArea")
  
  # 4. Función para realizar las pruebas de normalidad en una variable
  test_normalidad <- function(variable, nombre_variable) {
    cat("========================================================\n")
    cat("Pruebas de normalidad para:", nombre_variable, "\n")
    cat("--------------------------------------------------------\n")
    
    # Eliminar valores faltantes
    x <- na.omit(variable)
    
    # Verificar que haya suficientes observaciones
    if (length(x) < 3) {
      cat("No hay suficientes datos para realizar pruebas en", nombre_variable, "\n")
      return(NULL)
    }
    
    # Prueba de Shapiro-Wilk
    sw <- shapiro.test(x)
    cat("Shapiro-Wilk test:\n")
    print(sw)
    
    # Prueba de Anderson-Darling
    ad <- ad.test(x)
    cat("\nAnderson-Darling test:\n")
    print(ad)
    
    # Prueba de Kolmogorov-Smirnov
    # Se compara con la distribución normal utilizando la media y desviación estándar estimadas
    ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
    cat("\nKolmogorov-Smirnov test:\n")
    print(ks)
    
    # Prueba de Lilliefors (corrección del KS)
    lillie <- lillie.test(x)
    cat("\nLilliefors test:\n")
    print(lillie)
    
    cat("========================================================\n\n")
  }
  
  # 5. Aplicar las pruebas para cada grupo de variables
  
  cat("***** Grupo 1: Variable objetivo y áreas *****\n\n")
  for (var in vars_group1) {
    test_normalidad(datos[[var]], var)
  }
  
  cat("***** Grupo 2: Variables de calidad y construcción *****\n\n")
  for (var in vars_group2) {
    test_normalidad(datos[[var]], var)
  }
  
  cat("***** Grupo 3: Variables relacionadas con acabados y garaje *****\n\n")
  for (var in vars_group3) {
    test_normalidad(datos[[var]], var)
  }
  
  
  
  # Preguntas
  
  
  
  
 # 1.  **¿Cómo se relacionan las variables de área (GrLivArea, LotArea, X1stFlrSF, TotalBsmtSF) con el precio de venta y cómo varían estas relaciones según categorías de calidad (OverallQual, OverallCond) y ubicación (Neighborhood, MSZoning)?**
    
  # Convertir variables de interés a factor
  datos$OverallQual  <- as.factor(datos$OverallQual)
  datos$OverallCond  <- as.factor(datos$OverallCond)
  datos$Neighborhood <- as.factor(datos$Neighborhood)
  datos$MSZoning     <- as.factor(datos$MSZoning)
  
  # Gráficos para cada variable de área
  
  # GrLivArea vs SalePrice, coloreado por OverallQual
  p1 <- ggplot(datos, aes(x = GrLivArea, y = SalePrice, color = OverallQual)) +
    geom_point(alpha = 0.6) +
    labs(title = "GrLivArea vs SalePrice\npor OverallQual",
         x = "GrLivArea", y = "SalePrice") +
    theme_minimal()
  
  # LotArea vs SalePrice, coloreado por OverallCond
  p2 <- ggplot(datos, aes(x = LotArea, y = SalePrice, color = OverallCond)) +
    geom_point(alpha = 0.6) +
    labs(title = "LotArea vs SalePrice\npor OverallCond",
         x = "LotArea", y = "SalePrice") +
    theme_minimal()
  
  # X1stFlrSF vs SalePrice, facetado por Neighborhood
  p3 <- ggplot(datos, aes(x = X1stFlrSF, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "darkgreen") +
    facet_wrap(~ Neighborhood) +
    labs(title = "X1stFlrSF vs SalePrice\npor Neighborhood",
         x = "X1stFlrSF", y = "SalePrice") +
    theme_minimal()
  
  # TotalBsmtSF vs SalePrice, facetado por MSZoning
  p4 <- ggplot(datos, aes(x = TotalBsmtSF, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "purple") +
    facet_wrap(~ MSZoning) +
    labs(title = "TotalBsmtSF vs SalePrice\npor MSZoning",
         x = "TotalBsmtSF", y = "SalePrice") +
    theme_minimal()
  
  # Mostrar los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  
  
 #  2.  **¿Qué impacto tienen los años de construcción y remodelación (YearBuilt, YearRemodAdd) en el precio? ¿Existen tendencias o agrupaciones de propiedades antiguas versus modernas que influyan en SalePrice?**
  # -----------------------------------------------------------
  # Pregunta 2: Impacto de los años de construcción y remodelación
  # en el precio (SalePrice)
  # -----------------------------------------------------------
  
  # Cargar librerías necesarias

  
  # Cargar el dataset (ajusta la ruta si es necesario)

  # Crear una variable que agrupe el año de construcción por década
  datos$DecadeBuilt <- floor(datos$YearBuilt / 10) * 10
  
  # 1. Scatter plot: YearBuilt vs SalePrice con línea de suavizado (tendencia lineal)
  p1 <- ggplot(datos, aes(x = YearBuilt, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    labs(title = "YearBuilt vs SalePrice",
         x = "Fecha de Construccion",
         y = "Precio de Venta") +
    theme_minimal()
  
  # 2. Boxplot: Distribución de SalePrice por Década de Construcción
  p2 <- ggplot(datos, aes(x = factor(DecadeBuilt), y = SalePrice)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "Distribución de SalePrice por Decada",
         x = "Decada de Construccion",
         y = "Precio de Venta") +
    theme_minimal()
  
  # 3. Scatter plot: YearRemodAdd vs SalePrice con línea de tendencia
  p3 <- ggplot(datos, aes(x = YearRemodAdd, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "purple") +
    geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
    labs(title = "YearRemodAdd vs SalePrice",
         x = "Remodeling Year",
         y = "Precio de Venta") +
    theme_minimal()
  
  # Organizar y mostrar los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, ncol = 2)
  
# 3.  **¿Cuáles son las diferencias en la distribución de precios entre los distintos tipos de construcción y estilos de vivienda (BldgType, HouseStyle), y qué patrones se observan en función de la estructura física de la propiedad?**
    
  # -----------------------------------------------------------
  # Pregunta 3: Análisis de la distribución de SalePrice según 
  # Building Type (BldgType) y House Style (HouseStyle)
  # -----------------------------------------------------------
  
  # Cargar librerías necesarias
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  
  # Cargar el dataset (ajusta la ruta si es necesario)
  datos <- read.csv("data/raw/train.csv", stringsAsFactors = FALSE)
  
  # Convertir las variables de interés a factor
  datos$BldgType   <- as.factor(datos$BldgType)
  datos$HouseStyle <- as.factor(datos$HouseStyle)
  
  # 1. Boxplot: SalePrice vs BldgType
  p1 <- ggplot(datos, aes(x = BldgType, y = SalePrice)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = "SalePrice by BldgType",
         x = "Building Type",
         y = "SalePrice") +
    theme_minimal()
  
  # 2. Boxplot: SalePrice vs HouseStyle
  p2 <- ggplot(datos, aes(x = HouseStyle, y = SalePrice)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "SalePrice by HouseStyle",
         x = "House Style",
         y = "SalePrice") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 3. Faceted Boxplots: SalePrice by combinación de BldgType y HouseStyle
  p3 <- ggplot(datos, aes(x = "", y = SalePrice)) +
    geom_boxplot(fill = "coral") +
    facet_grid(BldgType ~ HouseStyle) +
    labs(title = "SalePrice by BldgType and HouseStyle",
         x = "",
         y = "SalePrice") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Mostrar todos los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, ncol = 1)
  
  
  
#4.  **¿De qué manera afectan los acabados exteriores y materiales (Exterior1st, Exterior2nd, MasVnrType, MasVnrArea) la valoración de las viviendas? ¿Se observa que ciertos materiales o condiciones exteriores se asocian a precios más altos o más bajos?**
  # Convertir las variables categóricas a factor
  datos$Exterior1st <- as.factor(datos$Exterior1st)
  datos$Exterior2nd <- as.factor(datos$Exterior2nd)
  datos$MasVnrType  <- as.factor(datos$MasVnrType)
  
  # 1. Boxplot: SalePrice by Exterior1st
  p1 <- ggplot(datos, aes(x = Exterior1st, y = SalePrice)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = "SalePrice by Exterior1st", x = "Exterior1st", y = "SalePrice") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2. Boxplot: SalePrice by Exterior2nd
  p2 <- ggplot(datos, aes(x = Exterior2nd, y = SalePrice)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "SalePrice by Exterior2nd", x = "Exterior2nd", y = "SalePrice") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 3. Boxplot: SalePrice by MasVnrType
  p3 <- ggplot(datos, aes(x = MasVnrType, y = SalePrice)) +
    geom_boxplot(fill = "lightcoral") +
    labs(title = "SalePrice by MasVnrType", x = "MasVnrType", y = "SalePrice") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4. Scatter plot: MasVnrArea vs SalePrice with linear smooth
  p4 <- ggplot(datos, aes(x = MasVnrArea, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "purple") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    labs(title = "MasVnrArea vs SalePrice", x = "MasVnrArea", y = "SalePrice") +
    theme_minimal()
  
  # Mostrar los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  
      
#5.  **¿Cómo influyen las condiciones y características del sótano (BsmtQual, BsmtCond, BsmtFinType1, BsmtFinSF1, BsmtFinSF2) en el precio?¿Existe un efecto diferencial entre casas con sótanos terminados y sin terminar?**


  
  # Convertir variables categóricas a factor
  datos$BsmtQual <- as.factor(datos$BsmtQual)
  datos$BsmtCond <- as.factor(datos$BsmtCond)
  datos$BsmtFinType1 <- as.factor(datos$BsmtFinType1)
  
  # Crear variable para identificar si el sótano está terminado
  # Se asume que BsmtFinType1 == "Unf" (Unfinished) o NA indica sótano sin terminar.
  datos$BasementStatus <- ifelse(is.na(datos$BsmtFinType1) | datos$BsmtFinType1 == "Unf", 
                                 "Unfinished", "Finished")
  datos$BasementStatus <- as.factor(datos$BasementStatus)
  
  # 1. Boxplot: SalePrice vs BsmtQual
  p1 <- ggplot(datos, aes(x = BsmtQual, y = SalePrice)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = "SalePrice vs BsmtQual", x = "Basement Quality", y = "SalePrice") +
    theme_minimal()
  
  # 2. Boxplot: SalePrice vs BsmtCond
  p2 <- ggplot(datos, aes(x = BsmtCond, y = SalePrice)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "SalePrice vs BsmtCond", x = "Basement Condition", y = "SalePrice") +
    theme_minimal()
  
  # 3. Boxplot: SalePrice vs BsmtFinType1
  p3 <- ggplot(datos, aes(x = BsmtFinType1, y = SalePrice)) +
    geom_boxplot(fill = "lightcoral") +
    labs(title = "SalePrice vs BsmtFinType1", x = "Finished Basement Type", y = "SalePrice") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4. Scatter plot: BsmtFinSF1 vs SalePrice
  p4 <- ggplot(datos, aes(x = BsmtFinSF1, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "purple") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    labs(title = "BsmtFinSF1 vs SalePrice", x = "Basement Finished Area 1", y = "SalePrice") +
    theme_minimal()
  
  # 5. Scatter plot: BsmtFinSF2 vs SalePrice
  p5 <- ggplot(datos, aes(x = BsmtFinSF2, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "orange") +
    geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
    labs(title = "BsmtFinSF2 vs SalePrice", x = "Basement Finished Area 2", y = "SalePrice") +
    theme_minimal()
  
  # 6. Boxplot: SalePrice vs BasementStatus (Finished vs Unfinished)
  p6 <- ggplot(datos, aes(x = BasementStatus, y = SalePrice)) +
    geom_boxplot(fill = "wheat") +
    labs(title = "SalePrice by Basement Finished Status", x = "Basement Status", y = "SalePrice") +
    theme_minimal()
  
  # Mostrar todos los gráficos en una cuadrícula de 3 columnas
  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
  
#6.  **¿Qué rol juegan las variables relacionadas con el garaje (GarageType, GarageArea, GarageCars, GarageQual, GarageCond) en la determinación del precio de venta? ¿Están las propiedades con garajes de mejor calidad o mayor capacidad asociadas a precios superiores?**
  # -----------------------------------------------------------
  # Pregunta 6: Análisis del rol de variables relacionadas con el garaje
  # en la determinación de SalePrice.
  # Variables analizadas: GarageType, GarageArea, GarageCars, GarageQual, GarageCond
  # -----------------------------------------------------------
  
  # Cargar librerías necesarias

  
  # Convertir variables categóricas a factor
  datos$GarageType <- as.factor(datos$GarageType)
  datos$GarageQual <- as.factor(datos$GarageQual)
  datos$GarageCond <- as.factor(datos$GarageCond)
  
  # 1. Boxplot: SalePrice vs GarageType
  p1 <- ggplot(datos, aes(x = GarageType, y = SalePrice)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = "SalePrice by GarageType",
         x = "Garage Type",
         y = "SalePrice") +
    theme_minimal()
  
  # 2. Scatter plot: GarageArea vs SalePrice con línea de tendencia
  p2 <- ggplot(datos, aes(x = GarageArea, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "darkgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = "GarageArea vs SalePrice",
         x = "Garage Area",
         y = "SalePrice") +
    theme_minimal()
  
  # 3. Scatter plot: GarageCars vs SalePrice con línea de tendencia
  p3 <- ggplot(datos, aes(x = GarageCars, y = SalePrice)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
    labs(title = "GarageCars vs SalePrice",
         x = "Number of Cars",
         y = "SalePrice") +
    theme_minimal()
  
  # 4. Boxplot: SalePrice vs GarageQual
  p4 <- ggplot(datos, aes(x = GarageQual, y = SalePrice)) +
    geom_boxplot(fill = "lightcoral") +
    labs(title = "SalePrice by GarageQual",
         x = "Garage Quality",
         y = "SalePrice") +
    theme_minimal()
  
  # 5. Boxplot: SalePrice vs GarageCond
  p5 <- ggplot(datos, aes(x = GarageCond, y = SalePrice)) +
    geom_boxplot(fill = "lightyellow") +
    labs(title = "SalePrice by GarageCond",
         x = "Garage Condition",
         y = "SalePrice") +
    theme_minimal()
  
  # Organizar y mostrar los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
  
#7.  **¿Existen patrones de desequilibrio o baja representatividad en ciertas variables categóricas (por ejemplo, Alley, PoolQC, MiscFeature) que requieran agrupar categorías o realizar recodificaciones para un análisis más fiable?**
    
  # -----------------------------------------------------------
  # Pregunta 7: Evaluación de desequilibrio en variables categóricas
  # (Alley, PoolQC, MiscFeature) para identificar la necesidad
  # de recodificación o agrupación.
  # -----------------------------------------------------------
  

  
  # Convertir las variables a factor
  datos$Alley       <- as.factor(datos$Alley)
  datos$PoolQC      <- as.factor(datos$PoolQC)
  datos$MiscFeature <- as.factor(datos$MiscFeature)
  
  # Calcular las tablas de frecuencia
  freq_alley  <- table(datos$Alley, useNA = "ifany")
  freq_poolqc <- table(datos$PoolQC, useNA = "ifany")
  freq_misc   <- table(datos$MiscFeature, useNA = "ifany")
  
  # Mostrar las tablas de frecuencia en consola
  print(freq_alley)
  print(freq_poolqc)
  print(freq_misc)
  
  # Crear gráficos de barras para visualizar la distribución
  
  # Gráfico para Alley
  p1 <- ggplot(datos, aes(x = Alley)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Frequency of Alley", x = "Alley", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico para PoolQC
  p2 <- ggplot(datos, aes(x = PoolQC)) +
    geom_bar(fill = "salmon") +
    labs(title = "Frequency of PoolQC", x = "PoolQC", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico para MiscFeature
  p3 <- ggplot(datos, aes(x = MiscFeature)) +
    geom_bar(fill = "darkseagreen") +
    labs(title = "Frequency of MiscFeature", x = "MiscFeature", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostrar los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, ncol = 3)
  
  
  
  
#8.  **¿Cómo se comportan las variables relacionadas con la ubicación y configuración del terreno (LotShape, LandContour, Street, Utilities) y qué relación tienen con el precio de venta?**
    
  # Convertir variables categóricas a factor
  datos$LotShape    <- as.factor(datos$LotShape)
  datos$LandContour <- as.factor(datos$LandContour)
  datos$Street      <- as.factor(datos$Street)
  datos$Utilities   <- as.factor(datos$Utilities)
  
  # 1. Boxplot: SalePrice vs LotShape
  p1 <- ggplot(datos, aes(x = LotShape, y = SalePrice)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = "SalePrice by LotShape",
         x = "Lot Shape", y = "SalePrice") +
    theme_minimal()
  
  # 2. Boxplot: SalePrice vs LandContour
  p2 <- ggplot(datos, aes(x = LandContour, y = SalePrice)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "SalePrice by LandContour",
         x = "Land Contour", y = "SalePrice") +
    theme_minimal()
  
  # 3. Boxplot: SalePrice vs Street
  p3 <- ggplot(datos, aes(x = Street, y = SalePrice)) +
    geom_boxplot(fill = "lightcoral") +
    labs(title = "SalePrice by Street",
         x = "Street Type", y = "SalePrice") +
    theme_minimal()
  
  # 4. Boxplot: SalePrice vs Utilities
  p4 <- ggplot(datos, aes(x = Utilities, y = SalePrice)) +
    geom_boxplot(fill = "wheat") +
    labs(title = "SalePrice by Utilities",
         x = "Utilities", y = "SalePrice") +
    theme_minimal()
  
  # Mostrar las tablas de frecuencia en consola (opcional)
  print(table(datos$LotShape, useNA = "ifany"))
  print(table(datos$LandContour, useNA = "ifany"))
  print(table(datos$Street, useNA = "ifany"))
  print(table(datos$Utilities, useNA = "ifany"))
  
  # Organizar y mostrar los gráficos en una cuadrícula
  grid.arrange(p1, p2, p3, p4, ncol = 2) 
  
  
#9.  **¿Qué variables muestran mayor presencia de outliers o sesgo en su distribución, y cuál es el impacto de estos extremos en los modelos predictivos? ¿Es necesario aplicar transformaciones (como logaritmos) o segmentaciones específicas?**
  # -----------------------------------------------------------
  # Pregunta 9: Identificar variables con outliers y skewed distributions,
  # evaluar su impacto y considerar transformaciones.
  # Variables candidatas: SalePrice, GrLivArea, LotArea, TotalBsmtSF, GarageArea, MasVnrArea
  # -----------------------------------------------------------
  
  # Cargar librerías necesarias
  library(ggplot2)
  library(dplyr)
  library(moments)   # Para calcular skewness
  library(gridExtra)
  
  
  
  # Seleccionar las variables a evaluar
  vars <- c("SalePrice", "GrLivArea", "LotArea", "TotalBsmtSF", "GarageArea", "MasVnrArea")
  
  # Calcular skewness para cada variable
  skew_table <- data.frame(
    Variable = vars,
    Skewness = sapply(vars, function(v) {
      skewness(na.omit(datos[[v]]))
    })
  )
  print(skew_table)
  
  # Generar histogramas y boxplots para cada variable
  plot_list <- lapply(vars, function(v) {
    # Histograma de la variable
    p_hist <- ggplot(datos, aes_string(x = v)) +
      geom_histogram(fill = "skyblue", bins = 30, color = "black") +
      labs(title = paste("Histogram of", v),
           x = v, y = "Frequency") +
      theme_minimal()
    
    # Boxplot de la variable
    p_box <- ggplot(datos, aes_string(y = v)) +
      geom_boxplot(fill = "orange", outlier.colour = "red", outlier.shape = 16) +
      labs(title = paste("Boxplot of", v),
           y = v) +
      theme_minimal()
    
    list(p_hist = p_hist, p_box = p_box)
  })
  
  # Ejemplo: Mostrar gráficos para las dos primeras variables (SalePrice y GrLivArea)
  grid.arrange(plot_list[[1]]$p_hist, plot_list[[1]]$p_box,
               plot_list[[2]]$p_hist, plot_list[[2]]$p_box,
               ncol = 2)
  
  # Opcional: Evaluar la transformación logarítmica para variables con valores positivos
  log_plot_list <- lapply(vars, function(v) {
    # Filtrar valores positivos para evitar problemas con log(0) o negativos
    data_nonzero <- datos %>% filter(.[[v]] > 0)
    p_log <- ggplot(data_nonzero, aes_string(x = paste0("log(", v, ")"))) +
      geom_histogram(fill = "lightgreen", bins = 30, color = "black") +
      labs(title = paste("Histogram of log(", v, ")", sep=""),
           x = paste("log(", v, ")", sep=""), y = "Frequency") +
      theme_minimal()
    p_log
  })
  
  # Mostrar los histogramas log-transformados en una cuadrícula
  do.call(grid.arrange, c(log_plot_list, ncol = 2))
  
#10. **¿Cómo se combinan las variables de calidad, área y ubicación para explicar de forma conjunta la variabilidad en el precio de las propiedades?**
    