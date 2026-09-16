# R/modeling/split_and_prep.R

SEED <- 12578891
set.seed(SEED)

# remove redundant variables, remove rows with NAs
# - lifeboat determines survival, data leakage
# - siblings_spouses and parents_children are included in family_size;
#   this was done to reduce the dimension of the dataset
# - is_child, age_group, family_size_group are duplicating data;
#   most models prefer numerical variables
titanic_modeling <- titanic_clean %>% 
  select(survived, class, sex, fare, embarked, age, family_size) %>% 
  drop_na()

# split the titanic_clean dataset, 80/20 proportion 
train_index <- createDataPartition(titanic_modeling$survived,
                                   p = 0.8,
                                   list = FALSE)

titanic_train <- titanic_modeling[train_index, ]
titanic_test <- titanic_modeling[-train_index, ]