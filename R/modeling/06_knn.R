# R/modeling/06_knn.R

set.seed(SEED)

datasets_knn <- list(
  'Full Features' = titanic_train,
  '2D MDS' = mds_train_extended %>% select(survived, x, y),
  '2D PCA' = pca_train_extended %>% select(survived, PC1, PC2),
  'Full + 2D MDS' = titanic_train %>% bind_cols(mds_train_extended %>% select(x, y))
)

knn_models <- lapply(names(datasets_knn), function(name) {
  set.seed(SEED)
  train(
    survived ~ .,
    data = datasets_knn[[name]],
    method = 'knn',
    preProcess = c('center', 'scale'),
    trControl = cv_control,   # same as in 05_log_regression.R
    metric = 'ROC',
    tuneLength = 10
  )
})
names(knn_models) <- names(datasets_knn)

knn_performance <- evaluate_supervised_methods(knn_models)