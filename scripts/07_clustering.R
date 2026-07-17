## ============================================================
## PHASE 7: CLUSTERING
## ============================================================
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

crime <- read.csv("data/crime_transformed.csv", stringsAsFactors = FALSE)
main_cats <- c("Murder","Attempted Murder","Kidnapping/Abduction","Dacoity",
               "Robbery","Burglary","Cattle Theft","Other Theft","Miscellaneous")

## ---- 7.1 Build category x month matrix (each category = one row) ----
wide_cat <- crime %>% filter(Category %in% main_cats) %>%
  select(Date, Category, Cases) %>%
  pivot_wider(names_from = Date, values_from = Cases) %>%
  as.data.frame()
rownames(wide_cat) <- wide_cat$Category
wide_cat$Category <- NULL

## ---- 7.2 Standardize (z-score) each category's monthly profile -------
scaled <- scale(wide_cat)

## ---- 7.3 Determine optimal number of clusters (elbow method) ---------
p_elbow <- fviz_nbclust(scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")
ggsave("figures/19_elbow.png", p_elbow, width = 6, height = 4, dpi = 150)

## ---- 7.4 K-means clustering (k = 3) -----------------------------------
set.seed(42)
km <- kmeans(scaled, centers = 3, nstart = 25)
print(km$cluster)
print(km$centers)

cluster_assignment <- data.frame(Category = rownames(wide_cat), Cluster = km$cluster)
write.csv(cluster_assignment, "data/cluster_assignment.csv", row.names = FALSE)

## ---- 7.5 Visualize clusters via PCA -----------------------------------
p_clust <- fviz_cluster(km, data = scaled, repel = TRUE,
                         main = "K-Means Clustering of Crime Categories (PCA Projection)")
ggsave("figures/18_cluster_pca.png", p_clust, width = 7, height = 6, dpi = 150)

## ---- 7.6 Hierarchical clustering (dendrogram) as a cross-check --------
d <- dist(scaled)
hc <- hclust(d, method = "ward.D2")
png("figures/31_dendrogram.png", width = 800, height = 500)
plot(hc, main = "Hierarchical Clustering of Crime Categories", xlab = "", sub = "")
dev.off()

cat("\nPhase 7 complete: cluster assignments saved to data/cluster_assignment.csv\n")
cat("Cluster sizes:\n"); print(table(km$cluster))
