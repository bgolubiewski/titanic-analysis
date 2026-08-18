# R/01_import.R
titanic_raw <- read.csv('data/raw/titanic_raw.csv')

dim(titanic_raw)
names(titanic_raw)


# -- libraries --
# will be removed from this file eventually

library(tidyverse)
library(DataExplorer)
