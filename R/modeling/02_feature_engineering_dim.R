# R/modeling/02_feature_engineering_dim.R

# -----------------------------------------------------------------------------
# PCA (Train & Test)
# -----------------------------------------------------------------------------

# Data prep
train_pca_input <- titanic_train %>%
  select(-embarked, -sex, -survived) %>% 
  mutate(class = match(class, c('1st', '2nd', '3rd')))

test_pca_input <- titanic_test %>%
  select(-embarked, -sex, -survived) %>% 
  mutate(class = match(class, c('1st', '2nd', '3rd')))

# PCA, train subset
pca_model <- prcomp(
  train_pca_input,
  center = TRUE,
  scale. = TRUE
)
var_explained_train <- pca_model$sdev^2 / sum(pca_model$sdev^2)

# expanded dataframes with PCA coordinates
train_pca_coords <- as.data.frame(pca_model$x[, 1:2])
colnames(train_pca_coords) <- c('PC1', 'PC2')

test_pca_coords <- as.data.frame(predict(pca_model, newdata = test_pca_input)[, 1:2])
colnames(test_pca_coords) <- c('PC1', 'PC2')

pca_train_extended <- bind_cols(titanic_train, train_pca_coords)
pca_test_extended <- bind_cols(titanic_test, test_pca_coords)


# -----------------------------------------------------------------------------
# MDS (For train subset / Clustering)
# -----------------------------------------------------------------------------

titanic_mds_train <- titanic_train %>% select(-survived)

dissimilarity_matrix_train <- as.matrix(
  daisy(
    titanic_mds_train,
    type = list(ordratio="class"),
    metric = 'gower',
    stand = TRUE)
)

mds_model_train <- cmdscale(dissimilarity_matrix_train, k=2)
mds_train_extended <- titanic_train %>% 
  mutate(
    x = mds_model_train[, 1],
    y = mds_model_train[, 2]
  )
