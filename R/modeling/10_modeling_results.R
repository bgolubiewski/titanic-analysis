# R/modeling/10_modeling_results.R

# ============================================================
# Model Comparison
# ============================================================

model_comparison <- bind_rows(
  lr_performance %>% mutate(model = 'Logistic Regression'),
  knn_performance %>% mutate(model = 'k-Nearest Neighbours'),
  dt_performance %>%  mutate(model = 'Decision Tree'),
  rf_performance %>%  mutate(model = 'Random Forest'),
  svm_performance %>%  mutate(model = 'SVM (RBF)')
) %>% 
  group_by(model) %>% 
  slice_max(order_by = roc_auc, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  
  select(model, feature_set, roc_auc, accuracy, sensitivity, specificity, f1) %>% 
  arrange(desc(roc_auc))


metric_cols <- c('roc_auc', 'roc_auc', 'accuracy',
                 'sensitivity', 'specificity', 'f1')
model_comparison_formatted <- model_comparison %>% 
  highlight_max(cols = metric_cols, color = '#0056b3')


best_supervised_models <- list(
  'Random Forest' = rf_models[['Full Features']],
  'Logistic Regression' = lr_models[['Full + 2D MDS']],
  'SVM (RBF)' = svm_models[['Full Features']],
  'k-NN' = knn_models[['Full + 2D MDS']],
  'Decision Tree' = dt_models[['Full Features']]
)

# ============================================================
# Test subset calculations for the best model
# ============================================================


best_final_model <- rf_models[['Full Features']]

test_preds_class <- predict(best_final_model, newdata = titanic_test)
test_preds_prob <- predict(best_final_model, newdata = titanic_test, type = 'prob')

cm_test <- confusionMatrix(test_preds_class, titanic_test$survived, positive = 'Yes')

roc_test <- roc(
  response = titanic_test$survived,
  predictor = test_preds_prob$Yes,
  levels = c('No', 'Yes')
)

test_performance <- data.frame(
  evaluation = 'Test Set',
  roc_auc = round(as.numeric(roc_test$auc), 4),
  accuracy = round(unname(cm_test$overall['Accuracy']), 4),
  sensitivity = round(unname(cm_test$byClass['Sensitivity']), 4),
  specificity = round(unname(cm_test$byClass['Specificity']), 4),
  f1 = round(unname(cm_test$byClass['F1']), 4)
)

cv_rf_performance <- rf_performance %>% 
  filter(feature_set == 'Full Features') %>% 
  mutate(evaluation = 'Training Set (10-fold CV)') %>% 
  select(evaluation, roc_auc, accuracy, sensitivity, specificity, f1)

final_comparison_df <- bind_rows(cv_rf_performance, test_performance)
