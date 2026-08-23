# R/04_eda.R

# select quantitative variables
quant_vars <- names(titanic_clean)[sapply(titanic_clean, is.numeric)]
data_quant <- select(titanic_clean, all_of(quant_vars))

# select qualitative variables
qual_vars <- names(titanic_clean)[sapply(titanic_clean, is.factor)]
data_qual <- titanic_clean %>% 
  select(all_of(qual_vars))