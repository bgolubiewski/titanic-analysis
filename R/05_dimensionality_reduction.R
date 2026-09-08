# R/05_dimensionality_reduction.R


# --------------------------- PCA ----------------------------- #

# select quantitative variables, recode `class`; remove rows with NAs;
# survived used here to ensure clean NA removal
titanic_pca <- titanic_clean %>% 
  select(fare, age, siblings_spouses, parents_children, class, survived) %>%
  mutate(class = match(class, c('1st', '2nd', '3rd'))) %>% 
  drop_na()

pca <- prcomp(
  titanic_pca %>% select(fare, age, siblings_spouses, parents_children, class),
  center = TRUE,
  scale. = TRUE
)
var_explained <- pca$sdev^2 / sum(pca$sdev^2)

# combine PCA results with `survived`, used in plots
pca_df <- as.data.frame(pca$x) %>% 
  bind_cols(survived = titanic_pca$survived)


# --------------------------- MDS ----------------------------- #

# we omit lifeboat (data leakage), is_child, and family_size (duplicate data)
titanic_mds_prep <- titanic_clean %>%
  select(
    survived, class, sex, age, fare, siblings_spouses,
    parents_children, embarked, age_group, family_size_group
  ) %>% 
  drop_na()

# we remove the target variable and duplicate data;
# we do it after removing NAs, because those variables will be used in plots
titanic_mds_calc <- titanic_mds_prep %>% 
  select(-c(survived, age_group, family_size_group))

# calculate the dissimilarity matrix
dissimilarity_matrix <- as.matrix(
  daisy(
    titanic_mds_calc,
    type = list(ordratio="class"),
    metric = 'gower',
    stand = TRUE)
)

# perform MDS procedure, calculate STRESS (normalized)
mds_k2 <- cmdscale(dissimilarity_matrix, k=2)
mds_k3 <- cmdscale(dissimilarity_matrix, k=3)

dist_mds_k2 <- as.matrix(dist(mds_k2, method = 'euclidean'))
dist_mds_k3 <- as.matrix(dist(mds_k3, method = 'euclidean'))

stress_k2 <- sqrt(sum(
  (dissimilarity_matrix - dist_mds_k2)^ 2) / sum(dissimilarity_matrix^2))
stress_k3 <- sqrt(sum(
  (dissimilarity_matrix - dist_mds_k3)^ 2) / sum(dissimilarity_matrix^2))

# create dataframes for plotting - with survived, age_group, family_size_group
mds_k2_df <- titanic_mds_prep %>% 
  mutate(
    x = mds_k2[, 1],
    y = mds_k2[, 2]
  )

mds_k3_df <- titanic_mds_prep %>% 
  mutate(
    x = mds_k3[, 1],
    y = mds_k3[, 2],
    z = mds_k3[, 3]
  )

