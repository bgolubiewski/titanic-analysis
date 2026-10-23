# R/plots_and_tables/06_plots_modeling.R

# ============================================================
# Universal Plots
# ============================================================

draw_roc_curve <- function(models,
                           method_name = NULL,
                           custom_title = NULL) {
  
  roc_list <- lapply(models, function(m) {
    preds <- m$pred
    roc(
      response = preds$obs,
      predictor = preds$Yes,
      levels = c('No', 'Yes')
    )
  })
  
  title_text <- if (!is.null(custom_title)) custom_title else {
    paste0('ROC Curves - ', method_name)
  }
  
  ggroc(roc_list, size = 1.1, legacy.axes = FALSE) +
    geom_abline(
      intercept = 1,
      slope = 1,
      linetype = 'dashed',
      color = 'gray50',
      linewidth = 0.6
    ) +
    
    scale_color_brewer(palette = 'Set1') +
    labs(
      title = title_text,
      subtitle = 'Performance evaluation across feature spaces (10-fold CV)',
      x = 'Specificity',
      y = 'Sensitivity',
      color = 'Feature Set'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
}


draw_model_confusion_matrix <- function(model,
                                        custom_subtitle = NULL,
                                        custom_title = NULL) {
  
  title_text <- if (!is.null(custom_title)) custom_title else {
    paste0('Confusion Matrix')
  }
  
  subtitle_text <- if (!is.null(custom_subtitle)) custom_subtitle else {
    paste0('10-fold CV')
  }
  
  preds <- model$pred
  cm <- confusionMatrix(preds$pred, preds$obs, positive = 'Yes')
  df_cm <- as.data.frame(cm$table)
  
  colnames(df_cm) <- c('Predicted', 'Actual', 'Freq')
  
  total_obs <- sum(df_cm$Freq)
  df_cm <- df_cm %>% 
    mutate(
      pct = Freq / total_obs * 100,
      label = paste0(Freq, '\n', round(pct, 2), '%'),
      text_color = ifelse(Freq > max(Freq) * 0.4, 'white', 'gray15')
    )
  
  ggplot(df_cm, aes(x = Predicted, y = Actual, fill = Freq)) +
    
    geom_tile(color = 'white', linewidth = 1.5) +
    geom_text(
      aes(label = label, color = text_color),
      fontface = 'bold',
      size = 4.5,
      lineheight = 0.9
    ) +
    
    scale_color_identity() +
    scale_fill_gradient(
      low = "#DCEEFF",
      high = "#C6284F"
    ) +
    
    coord_fixed() +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = 'Predicted Class',
      y = 'Actual Class'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10), hjust = 0.5),
      
      axis.text = element_text(size = 11, face = "bold", color = "gray20"),
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

# ============================================================
# Model Specific Plots
# ============================================================

draw_knn_tuning <- function(knn_models_list) {
  
  # extract tuning results from all data variants
  tuning_df <- bind_rows(lapply(names(knn_models_list), function(name) {
    
    m <- knn_models_list[[name]]
    df <- m$results
    df$feature_set <- name
    
    best_k <- m$bestTune$k
    df$is_best <- df$k == best_k
    return(df)
  }))
  
  ggplot(tuning_df, aes(x = k, y = ROC,
                        color = feature_set, group = feature_set)) +
    
    geom_line(linewidth = 1) +
    
    geom_point(
      data = subset(tuning_df, !is_best),
      size = 2.2,
      alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_point(
      data = subset(tuning_df, is_best),
      shape = 21,
      size = 4.5,
      stroke = 1.3,
      fill = 'white',
      show.legend = FALSE
    ) +
    
    geom_text_repel(
      data = subset(tuning_df, is_best),
      aes(label = paste0('k = ', k)),
      fontface = 'bold',
      size = 3.5,
      direction = 'y',
      nudge_y = 0.02,
      segment.color = NA,
      show.legend = FALSE
    ) +
    
    scale_color_brewer(palette = 'Set1') +
    scale_x_continuous(breaks = unique(tuning_df$k)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    
    labs(
      title = 'k-NN Parameter Tuning',
      subtitle = 'ROC-AUC cross-validation score against number of neighbours (k)',
      x = 'Number of Neighbours (k)',
      y = 'ROC-AUC (10-fold CV)',
      color = 'Feature Set'
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      
      plot.margin = margin(30, 20, 20, 20),
      axis.title.x = element_text(margin = margin(t = 9), size = 10),
      axis.title.y = element_text(margin = margin(r = 9), size = 10),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
}

draw_decision_tree <- function(dt_model) {
  
  tree <- dt_model$finalModel
  
  par(mar = c(0.5, 0.5, 2, 0.5))
  rpart.plot(
    tree,
    type = 4,
    extra = 104,
    under = TRUE,
    box.palette = 'Blues',
    shadow.col = 'gray90',
    roundint = FALSE,
    tweak = 1.2,
    main = 'Decision Tree Structure (Full Features)'
  )
}

draw_dt_tuning <- function(dt_models_list) {
  
  # extract tuning results from all data variants
  tuning_df <- bind_rows(lapply(names(dt_models_list), function(name) {
    
    m <- dt_models_list[[name]]
    df <- m$results
    df$feature_set <- name
    
    best_cp <- m$bestTune$cp
    df$is_best <- abs(df$cp - best_cp) < 1e-7
    return(df)
  }))
  
  ggplot(tuning_df, aes(x = cp, y = ROC,
                        color = feature_set, group = feature_set)) +
    
    geom_line(linewidth = 1) +
    
    geom_point(
      data = subset(tuning_df, !is_best),
      size = 2.2,
      alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_point(
      data = subset(tuning_df, is_best),
      shape = 21,
      size = 4.5,
      stroke = 1.3,
      fill = 'white',
      show.legend = FALSE
    ) +
    
    geom_text_repel(
      data = subset(tuning_df, is_best),
      aes(label = paste0('cp = ', round(cp, 4))),
      fontface = 'bold',
      size = 3.5,
      direction = 'y',
      nudge_y = 0.03,
      segment.color = NA,
      show.legend = FALSE
    ) +
    
    scale_color_brewer(palette = 'Set1') +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    
    labs(
      title = 'Decision Tree Parameter Tuning',
      subtitle = 'ROC-AUC cross-validation score against Complexity Parameter (cp)',
      x = 'Complexity Parameter (cp)',
      y = 'ROC-AUC (10-fold CV)',
      color = 'Feature Set'
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      
      plot.margin = margin(30, 20, 20, 20),
      axis.title.x = element_text(margin = margin(t = 9), size = 10),
      axis.title.y = element_text(margin = margin(r = 9), size = 10),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
}

draw_rf_tuning <- function(rf_models_list) {
  
  # extract tuning results from all data variants
  tuning_df <- bind_rows(lapply(names(rf_models_list), function(name) {
    
    m <- rf_models_list[[name]]
    df <- m$results %>% 
      group_by(mtry) %>% 
      summarise(ROC = max(ROC, na.rm = TRUE), .groups = 'drop') %>% 
      mutate(feature_set = name)
    
    best_mtry <- m$bestTune$mtry
    df$is_best <- df$mtry == best_mtry
    return(df)
  }))
  
  ggplot(tuning_df, aes(x = mtry, y = ROC,
                        color = feature_set, group = feature_set)) +
    
    geom_line(linewidth = 1) +
    
    geom_point(
      data = subset(tuning_df, !is_best),
      size = 2.2,
      alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_point(
      data = subset(tuning_df, is_best),
      shape = 21,
      size = 4.5,
      stroke = 1.3,
      fill = 'white',
      show.legend = FALSE
    ) +
    
    geom_text_repel(
      data = subset(tuning_df, is_best),
      aes(label = paste0('mtry = ', round(mtry, 4))),
      fontface = 'bold',
      size = 3.5,
      direction = 'y',
      nudge_y = 0.02,
      segment.color = NA,
      show.legend = FALSE
    ) +
    
    scale_color_brewer(palette = 'Set1') +
    scale_x_continuous(breaks = unique(tuning_df$mtry)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    
    labs(
      title = 'Random Forest Parameter Tuning',
      subtitle = 'Optimal ROC-AUC cross-validation score against mtry across feature spaces',
      x = 'Number of Randomly Sampled Variables (mtry)',
      y = 'ROC-AUC (10-fold CV)',
      color = 'Feature Set'
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      
      plot.margin = margin(30, 20, 20, 20),
      axis.title.x = element_text(margin = margin(t = 9), size = 10),
      axis.title.y = element_text(margin = margin(r = 9), size = 10),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
}

draw_rf_importance <- function(rf_model) {
  
  imp_vector <- ranger::importance(rf_model$finalModel)
  
  imp_df <- data.frame(
    feature = names(imp_vector),
    importance = as.numeric(imp_vector) 
  ) %>% 
    arrange(importance) %>% 
    mutate(feature = factor(feature, levels = feature))
  
  ggplot(imp_df, aes(x = importance, y = feature)) +
    geom_col(fill = '#2b5c8f', width = 0.65) +
    
    geom_text(
      aes(label = round(importance, 2)),
      hjust = -0.2,
      size = 3.3,
      fontface = 'bold',
      color = 'gray20'
    ) +
    
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    
    labs(
      title = 'Random Forest — Feature Importance',
      subtitle = 'Variable importance based on mean decrease in Gini impurity',
      x = 'Variable Importance',
      y = NULL
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      
      plot.margin = margin(30, 20, 20, 20),
      axis.title.x = element_text(margin = margin(t = 9), size = 10),
      axis.title.y = element_text(margin = margin(r = 9), size = 10),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
}

draw_svm_tuning <- function(svm_models_list) {
  
  # extract tuning results from all data variants
  tuning_df <- bind_rows(lapply(names(svm_models_list), function(name) {
    
    m <- svm_models_list[[name]]
    df <- m$results %>% 
      group_by(C) %>% 
      filter(ROC == max(ROC, na.rm = TRUE)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      mutate(feature_set = name)
    
    best_C <- m$bestTune$C
    best_sigma <- m$bestTune$sigma
    
    df$is_best <- (abs(df$C - best_C) < 1e-7) & (abs(df$sigma - best_sigma) < 1e-7)
    return(df)
  }))
  
  ggplot(tuning_df, aes(x = C, y = ROC,
                        color = feature_set, group = feature_set)) +
    
    geom_line(linewidth = 1) +
    
    geom_point(
      data = subset(tuning_df, !is_best),
      size = 2.2,
      alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_point(
      data = subset(tuning_df, is_best),
      shape = 21,
      size = 4.5,
      stroke = 1.3,
      fill = 'white',
      show.legend = FALSE
    ) +
    
    geom_text_repel(
      data = subset(tuning_df, is_best),
      aes(label = paste0('C = ', round(C, 4), '\nσ = ', sigma)),
      fontface = 'bold',
      size = 3.5,
      direction = 'y',
      nudge_y = 0.025,
      segment.color = NA,
      show.legend = FALSE
    ) +
    
    scale_color_brewer(palette = 'Set1') +
    scale_x_log10(breaks = unique(tuning_df$C)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    
    labs(
      title = 'SVM (RBF Kernel) Parameter Tuning',
      subtitle = 'Optimal ROC-AUC cross-validation score across Cost (C) and Sigma (σ) Parameters',
      x = 'Cost Parameter (C, log10 scale)',
      y = 'ROC-AUC (10-fold CV)',
      color = 'Feature Set'
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      
      plot.margin = margin(30, 20, 20, 20),
      axis.title.x = element_text(margin = margin(t = 9), size = 10),
      axis.title.y = element_text(margin = margin(r = 9), size = 10),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
}

# ============================================================
# Test Subset Plots
# ============================================================

draw_comparative_roc <- function(best_models_list) {
  
  roc_list <- lapply(best_models_list, function(m) {
    preds <- m$pred
    
    if (!is.null(m$bestTune)) {
      preds <- inner_join(preds, m$bestTune, by = names(m$bestTune))
    }
    
    roc(
      response = preds$obs,
      predictor = preds$Yes,
      levels = c('No', 'Yes')
    )
  })
  
  names(roc_list) <- paste0(
    names(best_models_list),
    ' (AUC = ',
    sapply(roc_list, function(r) formatC(r$auc, digits = 4)),
    ')'
  )
  
  xlim = c(0, 0.6) 
  ylim = c(0.5, 1.0)
  
  ggroc(roc_list, size = 1.1, legacy.axes = FALSE) +
    geom_abline(
      intercept = 1,
      slope = 1,
      linetype = 'dashed',
      color = 'gray50',
      linewidth = 0.6
    ) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    scale_color_brewer(palette = 'Set1') +
    labs(
      title = 'ROC Curves Comparison — Supervised Learning',
      subtitle = 'Out-of-fold 10-fold CV performance for optimal algorithm configurations',
      x = 'Specificity',
      y = 'Sensitivity',
      color = 'Model'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10)),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      legend.position = "bottom",
      #legend.direction = "vertical",
      legend.title = element_text(face = "bold", size = 10),
      
      panel.grid.minor = element_blank(),
    )
  
  
}

draw_test_confusion_matrix <- function(model, test_data) {
  
  test_preds <- predict(model, newdata = test_data)
  actuals <- test_data[['survived']]
  
  cm <- confusionMatrix(test_preds, actuals, positive = 'Yes')
  df_cm <- as.data.frame(cm$table)
  colnames(df_cm) <- c('Predicted', 'Actual', 'Freq')
  
  total_obs <- sum(df_cm$Freq)
  df_cm <- df_cm %>% 
    mutate(
      pct = Freq / total_obs * 100,
      label = paste0(Freq, '\n', round(pct, 2), '%'),
      text_color = ifelse(Freq > max(Freq) * 0.4, 'white', 'gray15')
    )
  
  ggplot(df_cm, aes(x = Predicted, y = Actual, fill = Freq)) +
    
    geom_tile(color = 'white', linewidth = 1.5) +
    geom_text(
      aes(label = label, color = text_color),
      fontface = 'bold',
      size = 4.5,
      lineheight = 0.9
    ) +
    
    scale_color_identity() +
    scale_fill_gradient(
      low = "#DCEEFF",
      high = "#C6284F"
    ) +
    
    coord_fixed() +
    labs(
      title = 'Confusion Matrix — Test Set',
      subtitle = 'Evaluated on 178 unseen passenger observations',
      x = 'Predicted Class',
      y = 'Actual Class'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 10), hjust = 0.5),
      
      axis.text = element_text(size = 11, face = "bold", color = "gray20"),
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid = element_blank(),
      legend.position = "none"
    )
}