# R/plots_and_tables/03_plots_pca.R

draw_scree_plot <- function() {
  
  df_plot <- data.frame(
    PC = factor(paste0('PC', seq_along(var_explained)),
                levels = paste0('PC', seq_along(var_explained))),
    var = var_explained,
    cum_var = cumsum(var_explained)
  )
  
  ggplot(df_plot, aes(x = PC, group = 1)) +
    
    geom_col(aes(y = var), fill = '#2B5B84', alpha = 0.7, width = 0.5) +
    geom_text(
      aes(y = var, label = percent(var, accuracy = 0.1)),
      vjust = 1.5,
      color = 'white',
      fontface = 'bold',
      size = 3.2
    ) +
    
    geom_line(aes(y = cum_var), color = '#0B2545', linewidth = 1.1) +
    geom_point(aes(y = cum_var), color = '#0B2545', size = 3) +
    geom_label(
      aes(y = cum_var, label = paste0('Cumulative: ',
                                      percent(cum_var, accuracy = 0.1))),
      vjust = -0.4, size = 3.0, fontface = 'bold',
      fill = alpha('white', 0.9), label.size = 0.25
    ) +
    
    scale_y_continuous(
      labels = percent_format(),
      limits = c(0, 1.15),
      expand = c(0,0)
    ) +
    
    labs(
      title = 'Individual and Cumulative Variance Explained by PCA',
      x = 'Principal Component',
      y = 'Proportion of Variance'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_pca_heatmap <- function() {
  
  df_plot <- as.data.frame(pca$rotation) %>% 
    rownames_to_column(var = 'Variable') %>% 
    pivot_longer(cols = starts_with('PC'), names_to = 'PC', values_to = 'Loading')
  
  ggplot(df_plot, aes(x = PC, y = Variable, fill = Loading)) +
    
    geom_tile(color = 'white', linewidth = 0.8) +
    
    geom_text(
      aes(label = formatC(Loading, format = 'f', digits = 2)),
      fontface = 'bold',
      size = 3.5,
      color = if_else(abs(df_plot$Loading) > 0.5, 'white', 'black')
    ) +
    
    scale_fill_gradient2(
      low = '#17365D',
      mid = 'grey95',
      high = '#E45756',
      midpoint = 0,
      limits = c(-1, 1),
      name = 'Loading'
    ) +
    
    labs(
      title = 'PCA Component Loadings Heatmap',
      x = 'Principal Component',
      y = 'Original Variable'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_pca_scatterplot <- function(df = pca_df,
                                 var_explained = NULL,
                                 target_col = 'survived',
                                 custom_title = NULL) {
  
  pc1_label <- if(!is.null(var_explained)) paste0('PC1 (', percent(var_explained[1], accuracy = 0.1), ')') else 'PC1'
  pc2_label <- if(!is.null(var_explained)) paste0('PC2 (', percent(var_explained[2], accuracy = 0.1), ')') else 'PC2'
  
  var_label <- get_label(target_col)
  plot_title <- if(!is.null(custom_title)) custom_title else paste0('PCA projection by ', var_label)
  
  p <- ggplot(df, aes(x = PC1, y = PC2,
                      color = .data[[target_col]],
                      fill = .data[[target_col]])) +
    
    geom_point(alpha = 0.5, size = 2) +
    
    stat_ellipse(
      geom = 'polygon',
      alpha = 0.15,
      level = 0.95,
      linewidth = 0.8
    ) +
    
    labs(
      title = plot_title,
      x = pc1_label,
      y = pc2_label,
      color = var_label,
      fill = var_label
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (target_col == 'survived') {
    p <- p +
      scale_color_manual(values = c('Yes' = '#2B5B84', 'No' = '#E45756')) +
      scale_fill_manual(values = c('Yes' = '#2B5B84', 'No' = '#E45756'))
  } else {
    p <- p +
      scale_color_brewer(palette = 'Dark2') +
      scale_fill_brewer(palette = 'Dark2')
  }
  
  return(p)
}

draw_pca_biplot <- function() {
  
  pc1_label <- paste0("PC1 (", percent(var_explained[1], accuracy = 0.1), ")")
  pc2_label <- paste0("PC2 (", percent(var_explained[2], accuracy = 0.1), ")")
  
  fviz_pca_biplot(
    pca,
    col.ind = titanic_pca$survived,
    palette = c('#E45756', '#0B2545'),
    alpha.ind = 0.4,
    pointsize = 1.8,
    
    col.var = '#0B2545',
    label = 'var',
    repel = TRUE,
    title = 'PCA biplot'
  ) +
    
    labs(
      x = pc1_label,
      y = pc2_label,
      color = 'Survived',
      shape = 'Survived'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}