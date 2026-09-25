# R/modeling/09_svm.R

set.seed(SEED)

datasets_svm <- list(
  'Full Features' = titanic_train,
  '2D MDS' = mds_train_extended %>% select(survived, x, y),
  '2D PCA' = pca_train_extended %>% select(survived, PC1, PC2),
  'Full + 2D MDS' = titanic_train %>% bind_cols(mds_train_extended %>% select(x, y))
)

svm_grid <- expand.grid(
  sigma = c(0.01, 0.03, 0.05, 0.1, 0.2),
  C = c(0.25, 0.5, 1, 2, 4, 8, 16)
)


svm_models <- lapply(names(datasets_rf), function(name) {
  
  df <- datasets_rf[[name]]
  
  set.seed(SEED)
  train(
    survived ~ .,
    data = df,
    method = 'svmRadial',
    preProcess = c('center', 'scale'),
    
    trControl = cv_control,   # same as in 05_log_regression.R
    metric = 'ROC',
    tuneGrid = svm_grid
  )
})
names(svm_models) <- names(datasets_svm)

svm_performance <- evaluate_supervised_methods(svm_models)
