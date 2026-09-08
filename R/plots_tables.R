# R/plots_tables.R

# --------------------------------- Plots ------------------------------------ #

# --------------------- Distributions ------------------------- #

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
    count(.data[[var]], name='count') %>% 
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

# --------------------------- EDA ----------------------------- #

draw_survival_barplot <- function(var) {
  x_label <- get_label(var)
  
  df_plot <- data_qual %>% 
    filter(!is.na(.data[[var]]), !is.na(survived)) %>% 
    count(.data[[var]], survived, name = 'count') %>% 
    group_by(.data[[var]]) %>% 
    mutate(
      prop = count / sum(count)
    ) %>% 
    ungroup()
  
  angle_val <- if_else(var %in% c('age_group', 'family_size_group'), 45, 0)
  hjust_val <- if_else(angle_val > 0, 1, 0.5)
  
  ggplot(df_plot, aes(x=.data[[var]], y = prop, fill = survived)) +
    
    geom_col(
      position = 'fill',
      color = 'white',
      alpha = 0.9
    ) +
    
    geom_text(
      aes(label = percent(prop, accuracy = 0.1)),
      position = position_fill(vjust = 0.5),
      color = "white",
      size = 3.2,
      fontface = 'bold'
    ) +
    
    scale_y_continuous(
      labels = percent_format(accuracy = 1)
    ) +
    scale_fill_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D')
    ) +
    
    labs(
      title = paste0("Survival by '", var,"'"),
      x = x_label,
      y = NULL,
      fill = 'Survived'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(10,10,10,10),
      plot.title = element_text(face='bold', size=11, hjust=0.5),
      
      axis.text.x = element_text(angle = angle_val, hjust=hjust_val),
      
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA))
}

draw_survival_density <- function(var) {
  x_label <- get_label(var)
  
  df_plot <- titanic_clean %>% 
    filter(!is.na(.data[[var]]), !is.na(survived))
  
  title <- if (var == 'fare') {
    paste0("Distribution of '", var,"'", ' (log10)')
  } else {
    paste0("Distribution of '", var,"'")
  }
  
  p <- ggplot(df_plot, aes(x=.data[[var]],
                           color = survived,
                           fill = survived)) +
    
    geom_density(linewidth = 0.8, alpha = 0.4
    ) +
    
    scale_color_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D'),
      name = "Survived"
    ) +
    scale_fill_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D'),
      name = "Survived"
    ) +
    
    labs(
      title = title,
      x = x_label,
      y = NULL
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
      plot.background = element_rect(fill = "white", color = NA))
  
  if (var == 'fare') {
    p <- p + scale_x_log10(labels = dollar_format())
  }
  return(p)
}



draw_lifeboat_barplot <- function() {
  
  df_plot <- titanic_clean %>% 
    filter(!is.na(lifeboat), !is.na(class), !is.na(sex), lifeboat != 'NS') %>% 
    count(class, sex, name='count') %>%
    group_by(class) %>% 
    mutate(prop = count / sum(count)) %>% 
    ungroup()
  
  ggplot(df_plot, aes(x = class, y = count, fill = sex)) +
    geom_col(color = "#0B1F33", alpha = 0.8, width = 0.6) +
    
    geom_text(
      aes(label = percent(prop, accuracy = 0.1)),
      position = position_stack(vjust = 0.5),
      color = 'white',
      size = 3.2,
      fontface = 'bold'
    ) +
    
    scale_fill_manual(
      values = c('female' = '#E45756', 'male' = '#0B2545'),
      labels = c('female' = 'Female', 'male' = 'Male'),
      name = 'Sex'
    ) +
    
    labs(
      title = 'Lifeboat Passengers by Class and Sex',
      x = 'Passenger Class',
      y = 'Count'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(30,20,15,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      legend.position = 'right',
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_multivariate_heatmap <- function() {
  df_plot <- titanic_clean %>% 
    filter(!is.na(class), !is.na(age_group), !is.na(sex), !is.na(survived)) %>% 
    group_by(class, age_group, sex) %>% 
    summarise(
      survival_rate = mean(survived == 'Yes', na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    )
  
  ggplot(df_plot, aes(x = class, y = age_group, fill = survival_rate)) +
    
    geom_tile(
      color = 'white',
      linewidth = 0.8
    ) +
    
    geom_text(
      aes(label = percent(survival_rate, accuracy = 0.1)),
      color = 'white',
      fontface = 'bold',
      size = 3.8
    ) +
    
    facet_wrap(
      ~ sex,
      labeller = labeller(sex = c('female' = 'Females', 'male' = 'Males'))
    ) +
    
    scale_fill_gradientn(
      colors = c('#B83B5E', '#E69A8D', '#2B5B84'),
      labels = percent_format(),
      limits = c(0,1),
      name = 'Survival Rate'
    ) +
    
    labs(
      title = 'Survival Rate by Passenger Class and Age Group',
      x = "Passenger Class",
      y = "Age Category"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(30,20,15,20),
      plot.title = element_text(face = 'bold', hjust = 0.5, size = 11),
    
      legend.position = 'right',
      axis.title.y = element_text(margin = margin(r = 9), size = 10),
      axis.title.x = element_text(margin = margin(t = 9), size = 10),
      strip.text = element_text(face = 'bold', size = 10),
      
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}
  
draw_multivariate_boxplot <- function(x_var = 'class', y_var = 'fare') {
  
  x_label <- get_label(x_var)
  y_label <- get_label(y_var)
  
  if (y_var == 'fare') {
    y_label <- paste0(y_label, ' (log10)')
  }
  
  df_plot <- titanic_clean %>% 
    filter(!is.na(.data[[x_var]]),
           !is.na(.data[[y_var]]),
           !is.na(survived))
    
  
  p <- ggplot(df_plot, aes(x = .data[[x_var]], y = .data[[y_var]],
                           fill = survived)) +
    geom_boxplot(
      color = "#17365D",
      alpha = 0.8,
      outlier.shape = 21,
      outlier.size = 1.5,
      outlier.alpha = 0.6,
      position = position_dodge(width = 0.8)
    ) +
    
    stat_summary(
      fun = mean,
      geom = 'point',
      shape = 18,
      size = 2.5,
      color = "gold",
      position = position_dodge(width = 0.8),
      show.legend = FALSE
    ) +
    
    scale_fill_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D'),
      name = 'Survived'
    ) +
    
    labs(
      title = paste0("Fare Distribution Across Classes"),
      x = x_label,
      y = y_label
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = 'bold', size = 11, hjust = 0.5),
      plot.margin = margin(30, 20, 15, 20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      legend.position = 'right',
      
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (y_var == 'fare') {
    p <- p + scale_y_log10(labels = dollar_format(prefix = '£'))
  }
  return(p)
}

draw_multivariate_family <- function() {
  df_plot <- titanic_clean %>% 
    filter(!is.na(family_size), !is.na(sex), !is.na(survived)) %>% 
    group_by(family_size, sex) %>% 
    summarise(
      survival_rate = mean(survived == 'Yes', na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    )
  
  ggplot(df_plot, aes(x = family_size, y = survival_rate,
                      color = sex, group = sex)) +
    
    geom_line(linewidth = 1.1) +
    geom_point(size = 3.5) +
    
    geom_label(
      aes(
        label = percent(survival_rate, accuracy = 0.1),
        vjust = if_else(sex == 'female', -0.4, 1.4)),
      size = 3.2,
      fontface = 'bold',
      fill = alpha('white', 0.9),
      label.size = 0.25,
      label.r = unit(0.15, 'lines'),
      label.padding = unit(0.14, 'lines'),
      show.legend = FALSE
    ) +
    
    scale_x_continuous(
      breaks = seq(min(df_plot$family_size), max(df_plot$family_size), by = 1)
    ) +
    scale_y_continuous(
      labels = percent_format(),
      expand = expansion(mult = c(0.12, 0.18))
    ) +
    scale_color_manual(
      values = c('female' = '#E45756', 'male' = '#0B2545'),
      labels = c('female' = 'Female', 'male' = 'Male'),
      name = 'Sex'
    ) +
    
    labs(
      title = 'Survival Rate by Family Size and Sex',
      x = 'Family Size (members)',
      y = 'Survival Rate'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(20,20,20,20),
      plot.title = element_text(face='bold', size=11, hjust=0.5),
      
      legend.position = 'right',
      
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA))
}

draw_multivariate_embarked <- function() {
  df_plot <- titanic_clean %>% 
    filter(!is.na(embarked), !is.na(class), !is.na(sex), !is.na(survived)) %>% 
    group_by(class, embarked, sex) %>% 
    summarise(
      survival_rate = mean(survived == 'Yes', na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    )
  
  ggplot(df_plot, aes(x = survival_rate, y = class, color = embarked)) +
    
    geom_point(
      size = 4,
      alpha = 0.6,
      position = position_dodge(width = 0.5)
    ) +
    
    geom_text(
      aes(label = percent(survival_rate, accuracy = 0.1)),
      position = position_dodge(width = 0.5),
      vjust = -1.2,
      size = 3.0,
      fontface = 'bold',
      show.legend = FALSE
    ) +
    
    facet_wrap(
      ~ sex,
      labeller = labeller(sex = c('female' = 'Females', 'male' = 'Males'))
    ) +
    
    scale_x_continuous(
      labels = percent_format(),
      limits = c(0,1),
      expand = expansion(mult = c(0.1, 0.15))
    ) +
    
    scale_y_discrete(
      expand = expansion(mult = c(0.6, 0.6))
    ) +
    
    scale_color_manual(
      values = c('C' = '#163A5F', 'Q' = '#7A5195', 'S' = '#C49A3A'),
      labels = c('C' = 'Cherbourg', 'Q' = 'Queenstown', 'S' = 'Southampton'),
      name = 'Port of Embarkation'
    ) +
    
    labs(
      title = 'Survival Rate by Passenger Class and Embarkation Port',
      x = 'Survival Rate',
      y = 'Passenger Class'
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(20,20,20,20),
      plot.title = element_text(face='bold', size=11, hjust=0.5),
      panel.spacing = unit(2, "lines"),
      
      legend.position = 'right',
      
      panel.grid.major.x = element_line(color = 'gray95'),
      panel.grid.major.y = element_line(color = 'gray95'),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


# --------------------------- PCA ----------------------------- #

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

draw_pca_scatterplot <- function() {
  pc1_label <- paste0('PC1 (', percent(var_explained[1], accuracy = 0.1), ')')
  pc2_label <- paste0('PC2 (', percent(var_explained[2], accuracy = 0.1), ')')
  
  ggplot(pca_df, aes(x = PC1, y = PC2, color = survived, fill = survived)) +
    
    geom_point(alpha = 0.5, size = 2) +
    
    stat_ellipse(
      geom = 'polygon',
      alpha = 0.15,
      level = 0.95,
      linewidth = 0.8
    ) +
    
    scale_color_manual(
      values = c('Yes' = '#2B5B84', 'No' = '#E45756'),
      name = 'Survived') +
    scale_fill_manual(
      values = c('Yes' = '#2B5B84', 'No' = '#E45756'),
      name = 'Survived') +
    
    labs(
      title = 'PCA projection',
      x = pc1_label,
      y = pc2_label
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

# --------------------------- MDS ----------------------------- #

draw_shepard_plot <- function(k = 2) {
  
  mds_dist <- if (k == 2) dist_mds_k2 else dist_mds_k3
  stress_val <- if (k == 2) stress_k2 else stress_k3
  
  df_plot <- data.frame(
    original = as.vector(as.dist(dissimilarity_matrix)),
    mds = as.vector(as.dist(mds_dist))
  )
  
  ggplot(df_plot, aes(x = original, y = mds)) +
    
    geom_point(
      alpha=0.3,
      size=0.3,
      color='#2B5B84'
    ) +
    
    geom_abline(
      intercept = 0,
      slope = 1,
      color='#E45756',
      linetype='dashed'
    ) +
    
    labs(
      title = paste0("Shepard Diagram (k=", k, ")"),
      subtitle = paste("STRESS:", round(stress_val, 4)),
      x = "Original distance",
      y = "Distance after MDS mapping"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_mds_3d_static <- function() {
  
  color_map <- ifelse(mds_k3_df$survived == 'Yes', '#0B2545', '#E45756')
  
  scatter3D(
    mds_k3_df$x,
    mds_k3_df$y,
    mds_k3_df$z,
    
    colvar = NULL,
    col = color_map,
    
    xlab='MDS Dim 1',
    ylab='MDS Dim 2',
    zlab='MDS Dim 3',
    main = '3D MDS Representation by Survival',

    pch=16,
    colkey = FALSE,
    theta = 45,
    phi = 15,
    bty = "g")
  
    legend(
      'topright',
      legend = c('No', 'Yes'),
      col = c('#E45756', '#0B2545'),
      pch = 16,
      bty = "n",
      title = 'Survival Status')
}

draw_mds_2d <- function(var, show_ellipse = FALSE) {
  
  var_label <- get_label(var)
  is_numeric_var <- is.numeric(mds_k2_df[[var]])
  
  p <- if (is_numeric_var) {
    ggplot(mds_k2_df, aes(x = x, y = y, color = .data[[var]]))
  } else {
    ggplot(mds_k2_df, aes(x = x, y = y, color = .data[[var]], fill = .data[[var]]))
  }
    
  p <- p + 
    geom_point(
      alpha = 0.7,
      size = 1.8,
      stroke = 0
    ) +
    
    labs(
      title = paste0('2D MDS Representation by ', var_label),
      x = 'MDS Dimension 1',
      y = 'MDS Dimension 2',
      color = var_label,
      fill = if (is_numeric_var) NULL else var_label
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.margin = margin(10,20,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      
      legend.position = 'right',
      
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (show_ellipse) {
    p <- p + stat_ellipse(geom = 'polygon',
                          alpha = 0.05,
                          level = 0.95,
                          linewidth = 0.6)
  }
  
  if (is_numeric_var) {
    p <- p + scale_color_viridis_c(
      trans = if (var == 'fare') 'pseudo_log' else 'identity',
      breaks = if (var =='fare') c(0, 15, 50, 150, 500) else waiver(),
      labels = if (var == 'fare') label_currency(prefix = '£') else waiver(),
      option = 'viridis'
    )
  } else if (var == 'survived') {
    p <- p +
      scale_color_manual(values = c('No' = '#E45756', 'Yes' = '#0B2545')) +
      scale_fill_manual(values = c('No' = '#E45756', 'Yes' = '#0B2545'))
  } else {
    p <- p +
      scale_color_brewer(palette = 'Dark2') +
      scale_fill_brewer(palette = 'Dark2')
  }
  return(p)
}
    
draw_mds_3d_plotly <- function() {
  
  vars_to_show <- c("survived", "class", "sex", "age_group", "family_size_group", "fare")
  passenger_ids <- seq_len(nrow(mds_k3_df))
  
  survived_colors <- c('No' = '#E45756', 'Yes' = '#0B2545')
  palette_set1 <- brewer.pal(8, "Set1")
  
  p <- plot_ly()
  trace_var_map <- integer(0)
  
  for (i in seq_along(vars_to_show)) {
    var <- vars_to_show[i]
    var_label <- get_label(var)
    is_visible <- (i == 1)
    val <- mds_k3_df[[var]]
    
    if (is.numeric(val) && !is.factor(val)) {
      # 1. OBSŁUGA ZMIENNYCH CIĄGŁYCH (FARE)
      color_val <- if (var == "fare") log1p(val) else val
      hover_text <- paste0('Passenger: ', passenger_ids, '<br>', 
                           var_label, ': ', if (var == "fare") paste0("$", round(val, 2)) else val)
      
      # Ręczne ustawienie etykiet paska kolorów dla fare
      cb_config <- if (var == "fare") {
        fare_breaks <- c(0, 15, 50, 150, 500)
        list(
          title = var_label,
          tickvals = log1p(fare_breaks),
          ticktext = paste0("$", fare_breaks)
        )
      } else {
        list(title = var_label)
      }
      
      p <- p %>% add_trace(
        data = mds_k3_df,
        x = ~x, y = ~y, z = ~z,
        type = 'scatter3d',
        mode = 'markers',
        marker = list(
          size = 3.5,
          opacity = 0.8,
          color = color_val,
          colorscale = 'Viridis',
          reversescale = TRUE,
          colorbar = cb_config
        ),
        name = var_label,
        showlegend = FALSE,
        text = hover_text,
        hoverinfo = 'text+x+y+z',
        visible = is_visible
      )
      trace_var_map <- c(trace_var_map, i)
      
    } else {
      # 2. OBSŁUGA FAKTORÓW / ZMIENNYCH JAKOŚCIOWYCH
      levels_vec <- if (is.factor(val)) levels(val) else sort(unique(na.omit(val)))
      
      for (j in seq_along(levels_vec)) {
        lvl <- levels_vec[j]
        idx <- which(val == lvl)
        
        lvl_color <- if (var == 'survived') {
          survived_colors[as.character(lvl)]
        } else {
          palette_set1[(j - 1) %% length(palette_set1) + 1]
        }
        
        hover_text <- paste0('Passenger: ', passenger_ids[idx], '<br>', var_label, ': ', lvl)
        
        p <- p %>% add_trace(
          data = mds_k3_df[idx, ],
          x = ~x, y = ~y, z = ~z,
          type = 'scatter3d',
          mode = 'markers',
          marker = list(
            size = 3.5,
            opacity = 0.8,
            color = lvl_color
          ),
          name = as.character(lvl),
          legendgroup = var,
          showlegend = TRUE,
          text = hover_text,
          hoverinfo = 'text+x+y+z',
          visible = is_visible
        )
        trace_var_map <- c(trace_var_map, i)
      }
    }
  }
  
  buttons <- map(seq_along(vars_to_show), function(i) {
    vis_vec <- (trace_var_map == i)
    list(
      method = 'restyle',
      args = list('visible', vis_vec),
      label = get_label(vars_to_show[i])
    )
  })
  
  # Konfiguracja układu i blokada kliknięć w legendzie
  p %>% layout(
    title = list(text = "Interactive 3D MDS Embedding", font = list(size = 14)),
    scene = list(
      xaxis = list(title = "MDS Dim 1"),
      yaxis = list(title = "MDS Dim 2"),
      zaxis = list(title = "MDS Dim 3")
    ),
    legend = list(
      itemclick = FALSE,       # <--- Wyłącza ukrywanie kategorii po pojedynczym kliknięciu
      itemdoubleclick = FALSE  # <--- Wyłącza izolowanie kategorii po dwukrotnym kliknięciu
    ),
    updatemenus = list(
      list(
        type = "dropdown",
        direction = "down",
        x = 0.1,
        y = 1.15,
        showactive = TRUE,
        buttons = buttons
      )
    ),
    margin = list(l = 0, r = 0, b = 0, t = 40)
  )
}

# -------------------------------- Tables ------------------------------------ #

create_simple_table <- function(data, colnames) {
  kable(data,
        col.names = colnames,
        align = c('l', rep('c', times= length(colnames) - 1)),
        row.names = FALSE) %>% 
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE, position = 'center') %>%
    row_spec(0, bold = TRUE)
}

