# R/functions.R

# ------------------------------- Helpers ------------------------------------ #

# cramerV() uses complete cases for each pair of variables,
# similarly to 'pairwise.complete.obs' in correlations
calc_cramer_matrix <- function(df) {
  vars <- names(df)
  n <- length(vars)
  matrix <- matrix(1, n, n, dimnames = list(vars, vars))
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      value <- cramerV(df[[i]], df[[j]], bias.correct = TRUE)
      matrix[i, j] <- value
      matrix[j, i] <- value
    }
  }
  return(matrix)
}

# create labels for plots
quant_var_labels <- c(
  'age' = 'Age (years)',
  'fare' = 'Fare Price',
  'family_size' = 'Family Size (members)',
  'siblings_spouses' = 'Number of Siblings / Spouses',
  'parent_children' = 'Number of Parents / Children'
)

qual_var_labels <- c(
  'class' = 'Passenger Class',
  'sex' = 'Sex',
  'embarked' = 'Port of Embarkation',
  'lifeboat' = 'Lifeboat',
  'family_size_group' = 'Family Size Category',
  'age_group' = 'Age Category',
  'survived' = 'Survival Status',
  'is_child' = 'Child Status'
)

# returns the label associated with 'var' (key), falling back to the variable
# name if no matching label is found
get_label <- function(var) {
  label <- qual_var_labels[var] # if var not in vector, returns NA
  
  if (is.na(label)) {
    label <- quant_var_labels[var]
  }
  if (is.na(label) || is.null(label)) {
    return(var)
  } else {
    unname(label)
  }
}

# loads the Shepard diagram or generates and saves it if it does not exist
get_shepard_diagram <- function() {
  img_path <- here('resources', 'shepard_diagram.png')
  
  if (!file.exists(img_path)) {
    p1 <- draw_shepard_plot(k = 2)
    p2 <- draw_shepard_plot(k = 3)
    
    combined_plot <- p1 + p2
    ggsave(img_path, combined_plot, width = 10, height = 5, dpi = 300)
  }
  
  knitr::include_graphics(img_path)
}
