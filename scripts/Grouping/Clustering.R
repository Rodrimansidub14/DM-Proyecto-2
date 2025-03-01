# 1. Cargar las librerías necesarias
library(tidyverse)
library(cluster)
library(factoextra)
library(hopkins)
library(ggrepel)
library(GGally)

# 2. Lectura del dataset de viviendas (ajusta la ruta según corresponda)
datos <- read.csv("C:/Users/rodri/Documents/Data-Mining/DM-Proyecto-2/data/processed/train_preprocessed.csv", stringsAsFactors = FALSE)

# 3. Selección de variables relevantes para el clustering
# Se usan variables relacionadas con el precio, áreas, calidad y antigüedad
datos_clust <- datos %>% 
  select(SalePrice, GrLivArea, LotArea, OverallQual, YearBuilt, TotalBsmtSF, GarageArea)

# 4. Manejo de valores faltantes (eliminamos filas con NA en las variables seleccionadas)
datos_clust <- na.omit(datos_clust)

# 5. Transformación de variables con fuerte sesgo (aplicamos logaritmo)
datos_clust <- datos_clust %>%
  mutate(
    SalePrice_log    = log(SalePrice + 1),
    GrLivArea_log    = log(GrLivArea + 1),
    LotArea_log      = log(LotArea + 1),
    TotalBsmtSF_log  = log(TotalBsmtSF + 1),
    GarageArea_log   = log(GarageArea + 1)
  ) %>%
  # Seleccionamos las variables transformadas y las que no se transformaron
  select(SalePrice_log, GrLivArea_log, LotArea_log, OverallQual, YearBuilt, TotalBsmtSF_log, GarageArea_log)

# 6. Escalar las variables para que tengan el mismo peso
datos_scaled <- scale(datos_clust)
datos_scaled <- datos_scaled[apply(datos_scaled, 1, function(x) all(is.finite(x))), ]


# 7. Verificar la tendencia a clusterizar con el estadístico de Hopkins
set.seed(123)
hopkins_stat <- hopkins(datos_scaled)
print(paste("Hopkins statistic:", round(hopkins_stat, 4)))
# Un valor cercano a 0.5 indicaría aleatoriedad; valores cercanos a 0 o 1 sugieren estructura (la interpretación depende del contexto)
# ---- Muestreo para análisis ----
# Seleccionar una muestra (por ejemplo, 1120 observaciones) para agilizar el cálculo
# Suponiendo que 'datos_scaled' ya está definido (datos procesados y escalados)
set.seed(123)
n_sample <- min(1120, nrow(datos_scaled))
datos_sample <- datos_scaled[sample(seq_len(nrow(datos_scaled)), n_sample), ]

# Calcular la matriz de distancias Euclideana usando parDist
datos_dist <- parDist(as.matrix(datos_sample), method = "euclidean")

# Confirmar que es de clase "dist"
print(class(datos_dist))  # Debe imprimir "dist"

# Convertir la matriz de distancias a matriz, normalizar y reconvertir a 'dist'
dist_matrix <- as.matrix(datos_dist)
dist_matrix_norm <- (dist_matrix - min(dist_matrix)) / (max(dist_matrix) - min(dist_matrix))
datos_dist_norm <- as.dist(dist_matrix_norm)

# Verificar el rango de distancias normalizadas
print(range(dist_matrix_norm))  # Debería ser 0 a 1

# Visualizar la matriz de distancias con fviz_dist (VAT)
plot_vat_sample <- fviz_dist(datos_dist_norm, 
                             show_labels = FALSE,
                             gradient = list(low = "#005D8F",    
                                             mid2 = "#5CC6FF",    
                                             mid3 = "#FFFFFF",     
                                             mid4 = "#E01522",    
                                             high = "#780000")) +  
  ggtitle("VAT sobre muestra de 1120 observaciones") +
  theme_minimal() +
  scale_fill_gradientn(
    colors = c("#005D8F", "#5CC6FF", "#FFFFFF", "#E01522", "#780000"),
    values = scales::rescale(c(
      quantile(as.matrix(datos_dist_norm), 0.01),  
      quantile(as.matrix(datos_dist_norm), 0.25),  
      quantile(as.matrix(datos_dist_norm), 0.75),  
      quantile(as.matrix(datos_dist_norm), 0.99)
    )),
    name = "Distancia"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Mostrar la gráfica
print(plot_vat_sample)

# 8. Determinar el número óptimo de clusters usando el método del codo
fviz_nbclust(datos_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Método del Codo")
# (Se observa que el "codo" se aprecia en k = 4; ajústese según la gráfica)

# 9. Aplicar K-means con k = 4
set.seed(123)
k <- 4
km_res <- kmeans(datos_scaled, centers = k, nstart = 25)
print(km_res)

# 10. Agregar la asignación de clusters al dataset original (transformado)
datos_clust$Cluster <- as.factor(km_res$cluster)

# 11. Visualización de los clusters usando fviz_cluster
fviz_cluster(km_res, data = datos_scaled, ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal(),
             main = "Clusters K-means (k = 4)")

# 12. Análisis de la calidad del agrupamiento mediante el índice de silueta
sil <- silhouette(km_res$cluster, dist(datos_scaled))
fviz_silhouette(sil) + labs(title = "Gráfico de Silueta para Clustering")

# 13. Clustering jerárquico (opcional)
dist_matrix <- dist(datos_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
fviz_dend(hc, k = k, rect = TRUE, show_labels = FALSE,
          main = "Dendrograma - Clustering Jerárquico")

# 14. Análisis de Componentes Principales (PCA)
pca_res <- prcomp(datos_scaled, center = TRUE, scale. = TRUE)
summary(pca_res)
#
# 15. Visualización del Scree Plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 80),
         main = "Scree Plot - Porcentaje de Varianza Explicada")

# 16. Biplot de los dos primeros componentes
fviz_pca_biplot(pca_res, repel = TRUE,
                col.var = "red", col.ind = "blue",
                title = "Biplot PCA")

# 17. Contribución de las variables a los primeros componentes
fviz_contrib(pca_res, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribución a PC1")
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10) +
  labs(title = "Contribución a PC2")
