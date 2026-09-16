# R/modeling/03_pam.R

set.seed(SEED)

# ============================================================
# Full feature space
# ============================================================

# find the optimal K value, based on Silhouette metric
k_range <- 2:6
sil_values_pam <- data.frame(
  k = k_range,
  silhouette = sapply(k_range, function(k) {
    pam_fit <- pam(dissimilarity_matrix_train, k = k, diss = TRUE)
    pam_fit$silinfo$avg.width
  })
)
best_k_pam <- sil_values_pam$k[which.max(sil_values_pam$silhouette)] 

# fit PAM
pam_full_k2 <- pam(dissimilarity_matrix_train, diss = TRUE, k = best_k_pam)
pam_full_k6 <- pam(dissimilarity_matrix_train, diss = TRUE, k = 6)

# cluster assignments in the full feature space
mds_train_extended <- mds_train_extended %>% 
  mutate(
    cluster_pam_k2 = as.factor(pam_full_k2$clustering),
    cluster_pam_k6 = as.factor(pam_full_k6$clustering))

# identify the characteristics of clusters in the k=6 PAM
cluster_profiles_k6_pam <- titanic_train %>%
  mutate(cluster = pam_full_k6$clustering) %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    Pct_Survived = round(mean(survived == 'Yes') * 100, 1),
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

# PAM on 2D PCA coordinates
pam_pca_coords <- pca_train_extended %>% select(PC1, PC2)
pam_pca <- pam(pam_pca_coords, k = 2)

# PAM on 2D MDS coordinates
pam_mds_coords <- mds_train_extended %>% select(x, y)
pam_mds <- pam(pam_mds_coords, k = 2)

# cluster assignments in the reduced feature space
mds_train_extended <- mds_train_extended %>% 
  mutate(
    cluster_pam_mds = as.factor(pam_mds$clustering))

pca_train_extended <- pca_train_extended %>% 
  mutate(cluster_pam = as.factor(pam_pca$clustering))


# ============================================================
# Clustering performance summary
# ============================================================

pam_metrics_summary <- bind_rows(
  evaluate_clustering(mds_train_extended$cluster_pam_k2, mds_train_extended$survived, 'PAM (Full Gower Matrix, k = 2)'),
  evaluate_clustering(mds_train_extended$cluster_pam_k6, mds_train_extended$survived, 'PAM (Full Gower Matrix, k = 6)'),
  evaluate_clustering(pca_train_extended$cluster_pam, pca_train_extended$survived, 'PAM (2D PCA, k = 2)'),
  evaluate_clustering(mds_train_extended$cluster_pam_mds, mds_train_extended$survived, 'PAM (2D MDS, k = 2)')
)
