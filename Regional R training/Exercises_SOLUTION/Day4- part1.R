
###############################################################################
# Regional training on R software
# Amman - Jordan - 3-7 Dec 2023
###############################################################################

# About this script -------------------------------------------------------

# Day 4: Data visualization 
# Author: Basma AbdElGawad
# Purpose: import, check, process, and summarize cholera dataset for training purpose
# Date: 2023-12-02
# Version: 1.0



# Load packages ------------------------------------------------------------


pacman::p_load(
  rio,                    # to import/export different types of data
  here,                   # set relative path to project root
  data.table,             # fast to load large datasets
  janitor,                # data cleaning + quick tables
  dplyr,                  # data processing and manipulation
  skimr,
  
  slider,                 # for rolling moving average
  ggplot2,                 # data visualization
  gghighlight,            
  cowplot,                # dual axis
  scales,
  incidence,
  
  Hmisc,                  # label variables
  epikit,                 # age categories
  lubridate,              # date manipulation
  aweek,                  # date to week
  tidyverse               # data management
)


# ****************************** PARTE 1 *********************************#


# Import data -------------------------------------------------------------

## fread from data.table package ---- 
path <- "put here your file path and do not forget to reverse slash direction (\) to (/)"
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

Hmisc::label(cholera$bmi) = "kg/m2"

# ****************************** PARTE 4 *********************************#  


# Data processing 2 -------------------------------------------------------


### convert to date class ----
cholera <- cholera %>% 
  mutate(#adm_date= as.Date(adm_date, format = "%m-%d-%Y"),
    outcome_date= as.Date(outcome_date, origin= "1899-12-30"),
    # outcome_date= as_date(outcome_date, lubridate::origin)
    adm_date= mdy(adm_date)
  ) %>% 

mutate(epiweek= lubridate::epiweek(adm_date),
       epiweek_date1= floor_date(adm_date,
                                 unit = "week",
                                 week_start = 7),
       epiweek2= aweek::date2week(adm_date , week_start = "sun", floor_day = T),
       epiweek_date2= as.Date(epiweek2))


# epi curve ----

# daily epi curve

## with incidemce package

daily_epi <- incidence(dates = cholera$adm_date, interval = "day")

plot(daily_epi)  

## with ggplot2

daily_epi2 <- cholera %>% 
  ggplot(aes(adm_date))+
  geom_histogram(binwidth = 1)
daily_epi2

# weekly epi curve

## with incidence

weekly_epi <- incidence(dates = cholera$adm_date, interval = "Sunday week")

plot(weekly_epi) +
  labs(title= "Weekly distribution of reported cholera cases by admission date, May 2021- July 2023", 
       x= "Date", 
       y= "Number of cases", 
       caption = "Mock data for training purposes")


## with ggplot2

weekly_breaks <- seq.Date(
  from = floor_date(min(cholera$adm_date, na.rm = T), unit = "week", week_start = 7),
  to= ceiling_date(max(cholera$adm_date, na.rm = T), unit = "week", week_start = 7),
  by= "week"
)

weekly_epi2 <-cholera %>% 
  ggplot(aes(adm_date))+
  geom_histogram(breaks= weekly_breaks,
                 closed= "left")+
  labs(title= "Weekly distribution of reported cholera cases by admission date, May 2021- July 2023", 
       x= "Date", 
       y= "Number of cases", 
       caption = "Mock data for training purposes")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 14), 
        panel.border = element_rect(colour = "black", fill=NA, size=2))
weekly_epi2

