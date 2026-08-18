# R/03_features.R

titanic_clean <- titanic_clean %>% 
  mutate(
    # family size metrics
    family_size = siblings_spouses + parent_children + 1,
    
    family_size_group = case_when(
      family_size == 1 ~ 'alone',
      family_size <= 4 ~ 'small',
      family_size <= 7 ~ 'medium',
      TRUE ~ 'large'),
    family_size_group = as.factor(family_size_group),
    
    # age based features
    is_child = if_else(age < 18, 1, 0),
    is_child = as.numeric(is_child),
    
    age_group = case_when(
      age < 18 ~ 'child',
      age < 30 ~ 'young adult',
      age < 60 ~ 'adult',
      TRUE ~ 'senior'),
    age_group = as.factor(age_group)
  )