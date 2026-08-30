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


# --------------------------- MDS ----------------------------- #


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

