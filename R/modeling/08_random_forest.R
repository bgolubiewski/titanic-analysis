# R/modeling/08_random_forest.R

set.seed(SEED)

datasets_rf <- list(
  'Full Features' = titanic_train,
  '2D MDS' = mds_train_extended %>% select(survived, x, y),
  '2D PCA' = pca_train_extended %>% select(survived, PC1, PC2),
  'Full + 2D MDS' = titanic_train %>% bind_cols(mds_train_extended %>% select(x, y))
)

rf_models <- lapply(names(datasets_rf), function(name) {
  
  df <- datasets_rf[[name]]
  n_features <- ncol(df) - 1
  
  mtry_vals <- unique(pmin(n_features, c(1,2,3,4,5,6)))
  
  rf_grid <- expand.grid(
    mtry = mtry_vals,
    splitrule = 'gini',
    min.node.size = 20
  )
  
  set.seed(SEED)
  train(
    survived ~ .,
    data = df,
    method = 'ranger',
    importance = 'impurity',
    num.trees = 1000,
    
    trControl = cv_control,   # same as in 05_log_regression.R
    metric = 'ROC',
    tuneGrid = rf_grid
  )
})
names(rf_models) <- names(datasets_rf)

rf_performance <- evaluate_supervised_methods(rf_models)
