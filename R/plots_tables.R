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
                 outlier.alpha = 0.6) +
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
                 alpha = 0.7, linewidth = 0.8) +
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
      panel.grid.major.x = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), 
      
      plot.background = element_rect(fill = "white", color = NA)
    )
}

draw_distribution_barplot <- function(var) {
  x_label <- get_label(var)
  
  # Filter NAs in each column
  df_plot <- titanic_clean %>% 
    filter(!is.na(.data[[var]]))
  
  # variables which labels need rotating
  rotation_angles <- c(
    age_group = 45,
    family_size_group = 45,
    lifeboat = 90)
  
  angle_val <- unname(rotation_angles[var])
  if (is.na(angle_val)) angle_val <- 0
  
  hjust_val <- if_else(angle_val > 0, 1, 0.5)
  vjust_val <- if_else(angle_val == 90, 0.5, if_else(angle_val > 0, 1, 0.5))
  
  # draw a plot
  ggplot(df_plot, aes(x = .data[[var]])) +
    geom_bar(fill = "#B8CCE4",
             color = "#17365D",
             alpha = 0.8) +
    labs(
      title = paste0("Distribution of '", var, "'"),
      x = x_label,
      y = 'Count') +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,20,20),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      
      # Dynamic alignment and angle
      axis.text.x = element_text(
        angle = angle_val,
        hjust = hjust_val),
      
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
      alpha = 0.9) +
    geom_text(
      aes(label = percent(prop, accuracy = 0.1)),
      position = position_fill(vjust = 0.5),
      color = "white",
      size = 3.2,
      fontface = 'bold') +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(
      values = c('No' = '#5B7FA3', 'Yes' = '#17365D')) +
    labs(
      title = paste0("Survival by '", var,"'"),
      x = x_label,
      y = NULL,
      fill = 'Survived') +
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
    geom_density(linewidth = 0.8, alpha = 0.4) +
    scale_color_manual(values = c('No' = '#B8CCE4',
                                  'Yes' = '#17365D'),
                       name = "Survived") +
    scale_fill_manual(values = c('No' = '#B8CCE4',
                                 'Yes' = '#17365D'),
                      name = "Survived") +
    labs(
      title = title,
      x = x_label,
      y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(10,10,10,10),
      
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      panel.grid.major.x = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), 
      
      plot.background = element_rect(fill = "white", color = NA))
  
  if (var == 'fare') {
    p <- p + scale_x_log10(labels = dollar_format())
  }
  return(p)
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
      position = position_dodge(width = 0.8)) +
    stat_summary(
      fun = mean,
      geom = 'point',
      shape = 18,
      size = 2.5,
      color = "gold",
      position = position_dodge(width = 0.8),
      show.legend = FALSE) +
    scale_fill_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D'),
      name = 'Survived') +
    labs(
      title = paste0("Fare Distribution Across Classes"),
      x = x_label,
      y = y_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = 'bold', size = 11, hjust = 0.5),
      plot.margin = margin(20, 20, 20, 20),
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (y_var == 'fare') {
    p <- p + scale_y_log10(labels = dollar_format(prefix = '£'))
  }
  return(p)
}

draw_multivariate_barplot <- function(var = 'class') {
  x_label <- get_label(var)
  
  df_plot <- titanic_clean %>% 
    filter(!is.na(.data[[var]]),
           !is.na(survived),
           !is.na(sex)) %>% 
    count(sex, .data[[var]], survived, name='count') %>% 
    group_by(sex, .data[[var]]) %>% 
    mutate(prop = count / sum(count)) %>% 
    ungroup()
  
  ggplot(df_plot, aes(x = .data[[var]], y = prop, fill = survived)) +
    geom_col(
      position = 'fill'
    ) +
    facet_wrap(~ sex) +
    geom_text(
      aes(label = if_else(prop < 0.03, '', percent(prop, accuracy = 0.1))),
      position = position_fill(vjust = 0.5),
      color = 'white',
      size = 3.0,
      fontface = 'bold') +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D'), name = 'Survived') +
    labs(
      title = paste0("Survival Rate by Sex and Class"),
      x = x_label,
      y = 'Survival Rate') +
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(20,20,20,20),
      plot.title = element_text(face='bold', size=11, hjust=0.5),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA))
}

draw_multivariate_scatterplot <- function(x_var = 'age', y_var = 'fare') {
  
  x_label <- get_label(x_var)
  y_label <- get_label(y_var)
  
  if (y_var == 'fare') {
    y_label <- paste0(y_label, ' (log10)')
  }
  
  df_plot <- titanic_clean %>% 
    filter(!is.na(.data[[x_var]]),
           !is.na(.data[[y_var]]),
           !is.na(survived))
  
  p <- ggplot(df_plot, aes(x = .data[[x_var]],
                           y = .data[[y_var]],
                           fill = survived)) +
    geom_point(
      aes(fill = survived),
      shape = 21,
      size = 2.2,
      alpha = 0.7,
      stroke = 0.3,
      color = 'white') +
    geom_smooth(
      aes(color = survived),
      method = "loess",
      se = FALSE,
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = c('No' = '#E45756', 'Yes' = '#17365D'),
      name = "Survived"
    ) +
    scale_color_manual(
      values = c('No' = '#C23B3B', 'Yes' = '#0B2545'),
      guide = 'none'
    ) +
    labs(
      title = paste0('Age vs. Fare Price by Survival'),
      x = x_label,
      y = y_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = 'bold', size = 11, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10),
      axis.title.x = element_text(margin = margin(t = 8), size = 10),
      axis.title.y = element_text(margin = margin(r = 8), size = 10),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (y_var == 'fare') {
    p <- p + scale_y_log10(labels = dollar_format(prefix = '£'))
  }
  return(p)
}

# --------------------------- MDS ----------------------------- #


# -------------------------------- Tables ------------------------------------ #

create_data_type_table <- function(data) {
  kable(data,
        col.names = c('Variable', 'Type', 'NAs'),
        align = c('l','c','c'),
        row.names = FALSE) %>% 
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE, position = 'center') %>%
    row_spec(0, bold = TRUE)
}