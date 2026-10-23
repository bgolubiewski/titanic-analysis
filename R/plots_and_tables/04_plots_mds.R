# R/plots_and_tables/04_plots_mds.R

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

draw_mds_2d <- function(var,
                        df = mds_k2_df,
                        custom_title = NULL,
                        xlab = 'MDS Dimension 1',
                        ylab = 'MDS Dimension 2',
                        show_ellipse = FALSE) {
  
  var_label <- get_label(var)
  is_numeric_var <- is.numeric(df[[var]])
  
  p <- if (is_numeric_var) {
    ggplot(df, aes(x = x, y = y, color = .data[[var]]))
  } else {
    ggplot(df, aes(x = x, y = y,
                   color = .data[[var]],
                   fill = .data[[var]]))
  }
  
  plot_title = if (!is.null(custom_title)) custom_title else {
    paste0('2D MDS Representation by ', var_label)}
  
  p <- p + 
    geom_point(
      alpha = 0.7,
      size = 1.8,
      stroke = 0
    ) +
    
    labs(
      title = plot_title,
      x = xlab,
      y = ylab,
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
      color_val <- if (var == "fare") log1p(val) else val
      hover_text <- paste0('Passenger: ', passenger_ids, '<br>', 
                           var_label, ': ', if (var == "fare") paste0("$", round(val, 2)) else val)
      

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
  
  p %>% layout(
    title = list(text = "Interactive 3D MDS Embedding", font = list(size = 14)),
    scene = list(
      xaxis = list(title = "MDS Dim 1"),
      yaxis = list(title = "MDS Dim 2"),
      zaxis = list(title = "MDS Dim 3")
    ),
    legend = list(
      itemclick = FALSE,
      itemdoubleclick = FALSE
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