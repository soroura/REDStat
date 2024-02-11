###############################################################################
# Regional training on R software
# Amman - Jordan - 3-7 Dec 2023
###############################################################################

# About this script -------------------------------------------------------

# Day 3: Data processing and summarization 
# Author: Basma AbdElGawad
# Purpose: import, check, process, and summarize cholera dataset for training purpose
# Date: 2023-11-07
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
  aweek,                  # date to week
  tidyverse               # data management
)


# ****************************** PARTE 1 *********************************#


# Import data -------------------------------------------------------------

## fread from data.table package ---- 
path <- "add your file path here with correct slash (/) direction"
cholera0 <- fread(file = paste0(path,"cholera_20231102.csv"), header = TRUE)


# ****************************** PART 2 *********************************#

# Data checking ----

#this is the usual section where we explore the imported dataset 

# ****************************** PARTE 3 *********************************#


# Data processing 1 -------------------------------------------------------

cholera <- cholera0 %>%     # piping cholera dataframe through all processing steps
  # clean names of all columns in a one go (check other possible arguments) 
  clean_names() %>% 
  rename(                  # manual renaming (!! new name = old name)
    case_cat = f_diag
  ) %>% 
  # modifying same column or creating new one using mutate
  mutate( sex= recode(sex,        
                      "f" = "female",
                      "F" = "female",
                      "Female" = "female",
                      "M" = "male",
                      "m" = "male",
                      "Male" = "male"
  ),
  age_yr = case_when(
    age_unit == "month" ~ round(age/12),
    .default = age
  ),
  age_cat = cut(age_yr,
        breaks = c(0,5,10,20,30,50,100),
        include.lowest = T,      # to include zero in the lowest group
        labels = c("0-5", "6-10", "11-20", "21-30","31-50", "51+")
  ),
  age_cat2 = age_categories(age_yr,
                            lower = 0,
                            upper = 60,
                            by= 10),
  bmi = round(wt_kg/(ht_cm/100)^2)
  ) 
# adding label to the bmi  
label(cholera$bmi) = "kg/m2"

  # ****************************** PARTE 4 *********************************#  
 

# Data processing 2 -------------------------------------------------------

## working with dates in R ----
  
### basic date functions ----
  
  #dates by base R
  Sys.time()
  Sys.Date()
  
  # dates by lubridate
  
  today()
  now()
  date()
  
  ### convert to date class ----
  cholera <- cholera %>% 
    # converting character to date
    mutate(#adm_date= as.Date(adm_date, format = "%m-%d-%Y"),
            adm_date= mdy(adm_date),
    # converting numeric to date
           outcome_date= as.Date(outcome_date, origin= "1899-12-30"),
    # converting different date class to date
    onset_date= ymd(onset_date)
                      ) %>% 
  
### extract data components ----

  # we may extract any date component from a class date vector
  # month("2023-11-12")
  # year("2023-11-12")
  # wday("2023-11-12",label = T, week_start = 7, abbr = F)
 
  # here we propose several ways to get epiweek for specific date 
  mutate(epiweek= lubridate::epiweek(adm_date),
         # start day of the week
         epiweek_date1= floor_date(adm_date,
                              unit = "week",
                              week_start = 7),
         epiweek2= aweek::date2week(adm_date , week_start = "sun", floor_day = T),
         epiweek_date2= as.Date(epiweek2))
  
  
### calculating difference between dates ----
  
  cholera <- cholera %>% 
    mutate(los = as.numeric(outcome_date - adm_date),
           sym_to_adm= as.numeric(adm_date - onset_date))

  
  ## working with factors ----
  
  ## sex: 1, Male | 2, Female | 3, Unknown
  cholera <- cholera %>% mutate( 
    # this step is only to recode sex (male/female) into numbers
    sex_f0 = case_when(sex == "female" ~ '2',
                       sex == "male" ~ '1',
                       TRUE ~ "unknown"),
    # create factor with base R function factor()
    sex_f = factor(sex_f0, levels = c(1,2,3), 
                   labels = c("Male", "Female", "Unknown"))
  )
  
  ## finished processing, clean variable levels that are not used
  cholera <- droplevels(cholera)

  
# export clean dataset ----
  
  ## base R function: write,csv ----
  write.csv(cholera, "path where you want to put your cleaned version/name of the file.csv")
  
  ## write_csv from readr package ----
  library(readr)
  write_csv(cholera, "path where you want to put your cleaned version/name of the file.csv")
  
  ## export + here from rio and here packages
  export(cholera, "path where you want to put your cleaned version/name of the file.csv")
  
  ## fwrite from data.table package ---- 
  path <- "path where you want to put your cleaned version/"
  fwrite(cholera, file = paste0(path,"name of the file.csv") )
  
  