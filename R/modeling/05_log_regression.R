# R/modeling/05_log_regression.R

set.seed(SEED)

cv_control <- trainControl(
  method = 'cv',
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = 'final'
)

datasets_lr <- list(
  'Full Features' = titanic_train,
  '2D MDS' = mds_train_extended %>% select(survived, x, y),
  '2D PCA' = pca_train_extended %>% select(survived, PC1, PC2),
  'Full + 2D MDS' = titanic_train %>% bind_cols(mds_train_extended %>% select(x, y))
)


lr_models <- lapply(names(datasets_lr), function(name) {
  set.seed(SEED)
  train(
    survived ~ .,
    data = datasets_lr[[name]],
    method = 'glm',
    family = 'binomial',
    preProcess = c('center', 'scale'),
    trControl = cv_control,
    metric = 'ROC'
  )
})
names(lr_models) <- names(datasets_lr)

lr_model_full <- lr_models[['Full Features']]
coef_summary <- summary(lr_model_full$finalModel)$coefficients
ci_matrix <- confint.default(lr_model_full$finalModel)

lr_coefficients <- data.frame(
  variable = rownames(coef_summary),
  odds_ratio = round(exp(coef_summary[, 'Estimate']), 3),
  ci_lower = round(exp(ci_matrix[, 1]), 2),
  ci_upper = round(exp(ci_matrix[, 2]), 2),
  p_value = round(coef_summary[, 'Pr(>|z|)'], 4)
) %>% 
  filter(variable != '(Intercept)') %>% 
  mutate(
    ci_95 = paste0('[', ci_lower, ', ', ci_upper, ']'),
    significance = case_when(
      p_value < 0.001 ~ '***',
      p_value < 0.01 ~ '**',
      p_value < 0.05 ~ '*',
      TRUE ~ 'ns'
    )
  ) %>% 
  select(variable, odds_ratio, ci_95, p_value, significance)


lr_performance <- evaluate_supervised_methods(lr_models)

lr_vif_results <- car::vif(lr_models[['Full + 2D MDS']]$finalModel)
#print(lr_vif_results)
