# R/modeling/04_agnes.R

set.seed(SEED)

# ============================================================
# Full feature space
# ============================================================

# compare linkage methods using AC
linkage_methods <- c('average', 'single', 'complete', 'ward')

ac_values_df <- data.frame(
  method = c('average', 'single', 'complete', 'ward'),
  ac = sapply(linkage_methods, function(m) {
  agnes(dissimilarity_matrix_train, diss = TRUE, method = m)$ac
  })
)

# fit AGNES using Ward's method
agnes_fit <- agnes(dissimilarity_matrix_train, diss = TRUE, method = 'ward')

# find optimal k value using the silhouette method
k_range <- 2:6
sil_values_agnes <- data.frame(
  k = k_range,
  silhouette = sapply(k_range, function(k) {
    clusters <- cutree(agnes_fit, k = k)
    sil_object <- silhouette(clusters, dissimilarity_matrix_train)
    mean(sil_object[, 'sil_width'])
  })
)

best_k_agnes <- sil_values_agnes$k[which.max(sil_values_agnes$silhouette)]


# cluster assignments in the full feature space
mds_train_extended <- mds_train_extended %>% 
  mutate(cluster_agnes_k2 = as.factor(cutree(agnes_fit, k = 2)),
         cluster_agnes_k6 = as.factor(cutree(agnes_fit, k = 6)))

# identify the characteristics of clusters in the k=6 and k=2 AGNES
cluster_profiles_k6_agnes <- titanic_train %>%
  mutate(cluster = cutree(agnes_fit, k = 6)) %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    Pct_Survived = round(mean(survived == "Yes") * 100, 1),
    Main_Sex = names(which.max(table(sex))),
    Main_Class = names(which.max(table(class))),
    Main_Embarked = names(which.max(table(embarked))),
    Median_Age = median(age),
    Median_Fare = round(median(fare), 1),
    Median_Family = median(family_size)
  )


# ============================================================
# Reduced feature space (PCA & MDS)
# ============================================================

# AGNES on 2D PCA coordinates
agnes_pca_coords <- pca_train_extended %>% select(PC1, PC2)
agnes_pca <- agnes(agnes_pca_coords, metric = 'euclidean', method = 'ward')

# AGNES on 2D MDS coordinates
agnes_mds_coords <- mds_train_extended %>% select(x, y)
agnes_mds <- agnes(agnes_mds_coords, metric = 'euclidean', method = 'ward')

# cluster assignments in the reduced feature space
pca_train_extended <- pca_train_extended %>% 
  mutate(cluster_agnes = as.factor(cutree(agnes_pca, k = 2)))

mds_train_extended <- mds_train_extended %>% 
  mutate(cluster_agnes_mds = as.factor(cutree(agnes_mds, k = 2)))


# ============================================================
# Clustering performance summary
# ============================================================

agnes_metrics_summary <- bind_rows(
  evaluate_clustering(mds_train_extended$cluster_agnes_k2, mds_train_extended$survived, 'AGNES (Full Gower Matrix, k = 2)'),
  evaluate_clustering(mds_train_extended$cluster_agnes_k6, mds_train_extended$survived, 'AGNES (Full Gower Matrix, k = 6)'),
  evaluate_clustering(pca_train_extended$cluster_agnes, pca_train_extended$survived, 'AGNES (2D PCA, k = 2)'),
  evaluate_clustering(mds_train_extended$cluster_agnes_mds, mds_train_extended$survived, 'AGNES (2D MDS, k = 2)')
)