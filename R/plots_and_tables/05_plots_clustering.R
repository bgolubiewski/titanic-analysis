# R/plots_and_tables/05_plots_clustering.R

draw_silhouette <- function(df = sil_values,
                            k_col = 'k',
                            sil_col = 'silhouette',
                            best_k = NULL,
                            custom_title = 'Optimal Number of Clusters (Silhouette Method)',
                            xlab = 'K (number of clusters)',
                            ylab = 'Mean Silhouette Value') {
  
  
  min_sil <- min(df[[sil_col]], na.rm = TRUE)
  
  ggplot(df, aes(x = .data[[k_col]], y = .data[[sil_col]])) +
    
    geom_line(color = '#0B2545', linewidth = 0.9) +
    geom_point(color = '#0B2545', size = 2.5) +
    
    geom_vline(
      xintercept = best_k,
      linetype = 'dashed',
      color = '#E45756',
      linewidth = 0.6
    ) +
    
    annotate('text',
             x = if_else(best_k == 6, best_k - 0.75, best_k + 0.15),
             y = min_sil,
             label = paste0('Optimal K = ', best_k), color = '#E45756',
             hjust = 0, vjust = 0, size = 3.5, fontface = 'bold'
    ) +
    
    scale_x_continuous(breaks = k_range) +
    
    labs(
      title = custom_title,
      x = xlab,
      y = ylab
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

draw_agnes_method_comparison <- function() {
  
  ggplot(ac_values_df, aes(x = reorder(method, ac), y = ac)) +
    
    geom_col(fill = '#0B2545', width = 0.6) +
    geom_text(aes(label = round(ac, 3)), vjust = -0.5,
              size = 3.5, fontface = 'bold') +
    
    scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) +
    labs(
      title = 'Agglomerative Coefficient (AC)',
      x = 'Linkage Method',
      y = 'AC Value'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

draw_agnes_dendrogram <- function(fit = agnes_fit,
                                  k = 2,
                                  custom_title = NULL,
                                  show_rect = TRUE,
                                  rect_fill = TRUE) {
  
  title_text <- if (!is.null(custom_title)) custom_title else {
    paste0('AGNES Dendrogram (Ward Linkage, K = ', k, ')')
  }
  
  palette <- if (k == 2) c('#E45756', '#0B2545') else 'Dark2'
  
  fviz_dend(
    x = fit,
    k = k,
    show_labels = FALSE,
    rect = show_rect,
    rect_fill = rect_fill,
    k_colors = palette,
    ggtheme = theme_minimal(base_size = 12)
  ) +
    
    scale_y_continuous(
      limits = c(0, 7.5),
      breaks = seq(0,7, by = 1),
      expand = c(0,0)
    ) +
    
    labs(
      title = title_text,
      x = 'Passenger Observations',
      y = 'Height (Dissimilarity)'
    ) +
    
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10, 10, 20, 20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      plot.background = element_rect(fill = "white", color = NA)
    )
}