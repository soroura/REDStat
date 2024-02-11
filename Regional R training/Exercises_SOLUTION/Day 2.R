###############################################################################
# Regional training on R software
# Amman - Jordan - 3-7 Dec 2023
###############################################################################

# About this script -------------------------------------------------------

# Day 2: Data importation and processing 
# Author: Basma AbdElGawad
# Purpose: import, check, and process cholera dataset for training purpose
# Date: 2023-11-01
# Version: 1.0



# Load packages ------------------------------------------------------------


pacman::p_load(
  rio,                    # to import/export different types of data
  here,                   # set relative path to project root
  data.table,             # fast to load large datasets
  
  janitor,                # data cleaning + quick tables
  dplyr,                  # data processing and manipulation
  skimr,
  Hmisc,                  # label variables
  epikit,                 # age categories
  lubridate,              # date manipulation
  tidyverse               # data management
)


# ****************************** PARTE 1 *********************************#


# Import data -------------------------------------------------------------

# several functions could be used to import data to R

## base R function: read,csv ----
cholera0 <- read.csv("file path in your computer")

## read_csv from readr package ----
library(readr)
cholera0 <- read_csv("file path in your computer")

## import + here from rio and here packages
cholera0 <-  import(here("Data/Cholera/cholera_20231102.csv"))

## fread from data.table package ---- 
path <- "file path in your computer"
cholera0 <- fread(file = paste0(path,"cholera_20231102.csv"), header = TRUE)


# ****************************** PART 2 *********************************#

# Data checking ----

dim(cholera0)       # dimensions of the data frame

nrow(cholera0)     # number of observations/rows in df
ncol(cholera0)     # number of variables/cols in df

str(cholera0)
skimr::skim(cholera0)     # quick description of each variable in the df
summary(cholera0)

head(cholera0)     # returns first 6 rows of the df
tail(cholera0)     # return last 6 rows of the df
names(cholera0)

class(cholera0)
class(cholera0$age)
class(cholera0$outcome_date)
class(cholera0$`adm-date`) # watch out for how adm-date wrapped in backticks

table(cholera0$country, useNA = "always")


# number of duplicated rows/observations
sum(duplicated(cholera0))

# latest onset date
max(cholera0$onset_date, na.rm = TRUE) #try it without na.rm= TRUE

# range and maen age of reported cases [!! watch out for age units]
mean(cholera0$age, na.rm = T)
range(cholera0$age, na.rm = T)
summary(cholera0$age)

# distribution of sex among reported cases

tabyl(cholera0, sex)
table(cholera0$sex, useNA = "always")

# name of reported countries
unique(cholera0$country)

# cross-tabulation of sex and f_diag

table(cholera0$sex, cholera0$f_diag, useNA = "always")


# ****************************** PARTE 3 *********************************#


# Data processing 1 -------------------------------------------------------

cholera <- cholera0 %>%     # piping cholera dataframe through all processing steps
  # clean names of all columns in a one go (check other possible arguments) 
  clean_names() %>% 
  rename(                  # manual renaming (!! new name = old name)
    case_cat = f_diag
  ) 
  # modifying same column or creating new one using mutate
 cholera <- cholera %>%  mutate( sex= recode(sex,
                                             "f" = "female",
                                             "F" = "female",
                                             "Female" = "female",
                                             "M" = "male",
                                             "m" = "male",
                                             "Male" = "male"))
 # creating age groups, age by year variable, 
 cholera <- cholera %>% mutate(age_yr = case_when(
    age_unit == "month" ~ round(age/12),
    .default = age
  ),
  age_cat = cut(age_yr,
        breaks = c(0,5,10,20,30,50,100),
        include.lowest = T,      # to include zero in the lowest group
        labels = c("0-5", "6-10", "11-20", "21-30","31-50", "51+")
  ),
  #using age_categories function to create the age group variable
  age_cat2 = age_categories(age_yr,
                            lower = 0,
                            upper = 60,
                            by= 10))
 
 #creating new variable called bmi, the calculation run using variables from the dataset
  cholera <- cholera %>%  mutate(bmi = round(wt_kg/(ht_cm/100)^2))  
  
  label(cholera$bmi)="kg/m2"
  
  #drop blank columns
  cholera <- cholera %>% select(-c(muac, ocv_doses, starts_with(c("rdt_", "cult_"))))

  

  # validating processing steps done ----
  
  # cross-tabulating old sex (cholera0) with new sex (cholera)
  
  table(cholera$sex, cholera0$sex, useNA = "always")
 
  # cross-tabulating age_yr with age groups
  table(cholera$age_yr, cholera$age_cat, useNA = "always")
  