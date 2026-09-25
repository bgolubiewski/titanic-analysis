# R/modeling/07_decision_tree.R

set.seed(SEED)

datasets_dt <- list(
  'Full Features' = titanic_train,
  '2D MDS' = mds_train_extended %>% select(survived, x, y),
  '2D PCA' = pca_train_extended %>% select(survived, PC1, PC2),
  'Full + 2D MDS' = titanic_train %>% bind_cols(mds_train_extended %>% select(x, y))
)

cp_grid <- expand.grid(cp = seq(0.002, 0.05, by = 0.002))

dt_models <- lapply(names(datasets_dt), function(name) {
  set.seed(SEED)
  train(
    survived ~ .,
    data = datasets_dt[[name]],
    method = 'rpart',
    trControl = cv_control,   # same as in 05_log_regression.R
    metric = 'ROC',
    tuneGrid = cp_grid
  )
})
names(dt_models) <- names(datasets_dt)

dt_performance <- evaluate_supervised_methods(dt_models)