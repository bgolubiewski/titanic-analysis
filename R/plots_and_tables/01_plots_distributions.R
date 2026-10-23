# R/plots_and_tables/01_plots_distributions.R

draw_distribution_boxplot <- function(var) {
  y_label <- get_label(var)
  
  ggplot(titanic_clean, aes(x = '', y = .data[[var]])) +
    
    geom_boxplot(fill = "#B8CCE4",
                 color = "#17365D",
                 alpha = 0.8,
                 outlier.shape = 16,
                 outlier.size = 1.5,
                 outlier.color = "#0B1F33",
                 outlier.alpha = 0.6
    ) +
    
    labs(
      title = paste0("Distribution of '", var, "'"),
      x = NULL,
      y = y_label
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 10), size = 10),
      
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_distribution_density <- function(var) {
  x_label <- get_label(var)
  
  ggplot(titanic_clean, aes(x = .data[[var]])) +
    
    geom_density(fill = "#B8CCE4", color = "#17365D",
                 alpha = 0.7, linewidth = 0.8
    ) +
    
    labs(
      title = paste0("Density of '", var, "'"),
      x = x_label,
      y = ''
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,10,10),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_distribution_barplot <- function(var) {
  x_label <- get_label(var)
  
  df_plot <- titanic_clean %>% 
    filter(!is.na(.data[[var]])) %>% 
    dplyr::count(.data[[var]], name='count') %>% 
    mutate(prop = count / sum(count))
  
  # variables which labels need rotating
  rotation_angles <- c(
    age_group = 45,
    family_size_group = 45,
    lifeboat = 90)
  
  angle_val <- unname(rotation_angles[var])
  if (is.na(angle_val)) angle_val <- 0
  hjust_val <- if_else(angle_val > 0, 1, 0.5)
  
  ggplot(df_plot, aes(x = .data[[var]], y = count)) +
    geom_col(fill = "#17365D",
             color = "#0B1F33",
             alpha = 0.8) +
    
    geom_text(
      aes(label = percent(prop, accuracy = 0.1)),
      vjust = -0.4,
      color = '#17365D',
      size = 3.2,
      fontface = 'bold'
    ) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))
    ) +
    
    labs(
      title = paste0("Distribution of '", var, "'"),
      x = x_label,
      y = 'Count'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      # Dynamic alignment and angle
      axis.text.x = element_text(angle = angle_val, hjust = hjust_val),
      
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}