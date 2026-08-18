# R/02_cleaning.R
titanic_clean <- titanic_raw

# -- Remove unnecessary columns --
not_needed_names <- c('PassengerId', 'Cabin', 'WikiId',
                      'Hometown', 'Destination', 'Body', 'Ticket')

titanic_clean <- titanic_clean %>%
  select(-all_of(not_needed_names))

# -- Standardize missing values (NAs) --
#  - 'Fare': Zero fare is assumed to be missing data rather than free tickets,
#  - 'Embarked' & 'Boarded': Convert empty strings to NA,
#  - 'Lifeboat': Normalize irregular entries, fix typos, and code unboarded.
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
    Age_wiki  = if_else(is.na(Age_wiki),  mean(Age_wiki,  na.rm = TRUE), Age_wiki)) %>%
  ungroup()
    
# -- Remove not needed/duplicate columns
# Duplicates were removed based on number of NAs
colSums(is.na(titanic_clean))
titanic_clean <- titanic_clean %>%
  select(-c('Name_wiki', 'Name', 'Age', 'Class', 'Boarded'))

# -- Rename columns for better readability
names(titanic_clean)
titanic_clean <- titanic_clean %>% 
  rename(
    class = Pclass,
    siblings_spouses = SibSp,
    parent_children = Parch,
    age = Age_wiki,
    survived = Survived,
    sex = Sex,
    fare = Fare,
    embarked = Embarked,
    lifeboat = Lifeboat,
    #name = Name
  )

# -- Convert categorical attributes to factors
# 'Survived' remains numeric for easy probability/mean calculations
categorical_cols <- c('class', 'embarked', 'sex', 'lifeboat')

titanic_clean <- titanic_clean %>% 
  mutate(across(all_of(categorical_cols), as.factor))

glimpse(titanic_clean)
colSums(is.na(titanic_clean))
summary(titanic_clean)
