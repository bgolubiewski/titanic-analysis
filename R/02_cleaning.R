# R/02_cleaning.R

titanic_fixed <- read.csv(here('data', 'titanic_fixed.csv'))
titanic_clean <- titanic_fixed

# -- Remove unnecessary columns --
not_needed_names <- c('PassengerId', 'Cabin', 'WikiId',
                      'Hometown', 'Destination', 'Body', 'Ticket')

titanic_clean <- titanic_clean %>%
  select(-all_of(not_needed_names))

# -- Standardize missing values (NAs) --
#  - 'Fare': Zero fare is assumed to be missing data rather than free tickets,
#  - 'Embarked' & 'Boarded': Convert empty strings to NA,
#  - 'Lifeboat': Normalize irregular entries, fix typos.
#     Replace NA with 'NS' (No Survival) for passengers who did not survive.

titanic_clean <- titanic_clean %>% 
  mutate(
    Embarked = na_if(Embarked,''),
    Boarded = na_if(Boarded, ''),
    Fare = if_else(Fare == 0, NA_real_, Fare),
    
    Lifeboat = na_if(Lifeboat, ''),
    Lifeboat = na_if(Lifeboat, '?')
  )

titanic_clean$Lifeboat[titanic_clean$Lifeboat == "14?"] <- '14'
titanic_clean$Lifeboat[titanic_clean$Lifeboat == "A[64]"] <- 'A'
titanic_clean$Lifeboat[titanic_clean$Lifeboat == "15?"] <- '15'

titanic_clean$Lifeboat[titanic_clean$Survived == 0 &
                       is.na(titanic_clean$Lifeboat)] <- 'NS'

# -- Impute missing data --

# - 'Survival': we impute based on historical records
# Known lifeboat fatalities (in lifeboats 4, 14, A, B) were checked
# via Encyclopedia Titanica against missing 'Survived' records.

titanic_clean[
  is.na(titanic_clean$Survived) &
    !is.na(titanic_clean$Lifeboat) &
    titanic_clean$Lifeboat %in% c('4', '14', 'A', 'B'),
  c('Name_wiki', 'Lifeboat')]

# Since no such edge cases exist in this subset,
# we assume all lifeboat occupants survived.

titanic_clean <- titanic_clean %>% 
  mutate(
    Survived = if_else(is.na(Survived) &
                       !is.na(Lifeboat) &
                       Lifeboat != 'NS',
                       1, Survived))

# - 'Fare': we impute mean ticket price in passenger's class of travel
# - 'Age': impute mean age in passenger's class of travel
titanic_clean <- titanic_clean %>%
  group_by(Pclass) %>% 
  mutate(
    Fare = if_else(is.na(Fare), mean(Fare, na.rm = TRUE), Fare),
    Age_wiki  = if_else(is.na(Age_wiki),  ceiling(mean(Age_wiki,  na.rm = TRUE)),
                        Age_wiki)) %>%
  ungroup()

# - 'Boarded' and 'Embarked': both have a handful of NAs but the missing values
# do not occur in the same observations (df_temp),
# we impute 'Embarked' based on 'Boarded'

# df_temp <- titanic_clean %>% 
#   filter(is.na(Embarked) | is.na(Boarded)) %>% 
#   select(Embarked, Boarded)

titanic_clean <- titanic_clean %>% 
  mutate(
    Embarked = if_else(
      is.na(Embarked),
      substr(Boarded, 1, 1),
      Embarked))

# -- Remove not needed/duplicate columns --
# Duplicates were removed based on number of NAs
colSums(is.na(titanic_clean))
titanic_clean <- titanic_clean %>%
  select(-c('Name_wiki', 'Name', 'Age', 'Class', 'Boarded'))

# -- Rename columns for better readability --
names(titanic_clean)
titanic_clean <- titanic_clean %>% 
  rename(
    class = Pclass,
    siblings_spouses = SibSp,
    parents_children = Parch,
    age = Age_wiki,
    survived = Survived,
    sex = Sex,
    fare = Fare,
    embarked = Embarked,
    lifeboat = Lifeboat,
    #name = Name
  )

# -- Convert categorical attributes to factors
categorical_cols <- c('class', 'embarked', 'sex', 'lifeboat', 'survived')

titanic_clean <- titanic_clean %>% 
  mutate(across(all_of(categorical_cols), as.factor))


## -- Fix factor levels and their names --

titanic_clean <- titanic_clean %>%
  mutate(
    class = factor(class, levels=c('1', '2', '3'),
                   labels = c('1st', '2nd', '3rd')),
    survived = factor(survived, levels = c(0,1), labels=c('No', 'Yes')),
    lifeboat = factor(lifeboat, levels = str_sort(unique(na.omit(lifeboat)),
                                                  numeric = TRUE))
  )

# --------------------------------------------------------------------------- #

glimpse(titanic_clean)
colSums(is.na(titanic_clean))
summary(titanic_clean)
