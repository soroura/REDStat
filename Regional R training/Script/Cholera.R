# About ----
#'Author:hossam
#'Date:04/12/2023
#'context: day two training


# Load package ------------------------------------------------------------



pacman::p_load(
  data.table,
  rio,
  here,
  dplyr,
  epikit,
  janitor,
  lubridate,
  ggplot2,
  crosstable,
  stringr,
  gtsummary,
  flextable,
  Hmisc,
  scales,
  incidence,
  tidyverse )



# Data source ----

cholera0 <-  import(here("Data/Cholera/cholera_20231102.csv"))

path <- "C:/Users/hramadan/Desktop/WHO-EG/WHO-EG/Training/R training/Jordan training Dec 2023/Regional R training/Data/Cholera/"

cholera0 <- fread(file = paste0(path,"cholera_20231102.csv"), header = TRUE)


# Data exploring ----

dim(cholera0)

str(cholera0)

skimr::skim(cholera0$sex)

class(cholera0)
class(cholera0$`adm-date`)
class(cholera0$outcome_date)

summarise(cholera0)

summary(cholera0)

sum(duplicated(cholera0))

min(cholera0$onset_date, na.rm = T)
max(cholera0$onset_date, na.rm = T)

range(cholera0$age)
mean(cholera0$age)
table(cholera0$sex,cholera0$f_diag)

table(cholera0$country)

unique(cholera0$country)

cholera1 <- fread ("C:/Users/hramadan/Desktop/WHO-EG/WHO-EG/Training/R training/Jordan training Dec 2023/Regional R training/Data/Cholera/cholera_20231102.csv", header = T)


# data processing ----
cholera <- cholera0 %>% janitor::clean_names() %>% 
  rename(case_cat = f_diag) %>% mutate( sex =
                                     recode(sex,
                                              "F" = "Female",
                                            "female" = "Female",
                                            "Female" = "Female",
                                            "m" = "Male",
                                            "male" = "Male",
                                            "Male" = "Male")) %>% 
  mutate(age_yr = case_when(age_unit == "month" ~ round(age/12,0), .default = age))

names(cholera)

table(cholera$sex)

cholera <- cholera %>% mutate(age_cat = age_categories(age_yr, 
                                                       lower = 0,
                                                       upper = 60,
                                                       by = 10))

# data processing2 ----

cholera <- cholera %>% #change the character date
  dplyr::mutate(adm_date2 = as.Date(adm_date, format = "%m-%d-%Y")) %>%
  dplyr::mutate(adm_date3 = mdy(adm_date), onset_date2 = ymd(onset_date))
                                    
                                                                          
cholera <- cholera %>% #change the numeric date
  dplyr::mutate(outcome_date2 = as.Date(outcome_date, origin= "1899-12-30"))

# creating epi-weeks

cholera <- cholera %>%
  dplyr::mutate(epiweek= aweek::date2week(adm_date2, week_start = 7, floor_day= 1)) %>% 
  dplyr::mutate(
    epiweek_start_date= aweek::week2date(epiweek)
  )
  
#LOS calculation

cholera <- cholera %>% 
  dplyr::mutate(los = as.numeric(outcome_date2 - adm_date2), 
                sym_to_adm= as.numeric(adm_date2 - onset_date2))


#create a factor

cholera <- cholera %>% 
  dplyr::mutate(sex_f0 = case_when(sex == "Female" ~ '2',
                                  sex== "Male" ~ '1',
                                  .default = "unknown"),
                sex_f = factor(sex_f0, levels= c(1,2,3),
                               label = c("Male","Female","unknown")))

cholera <- droplevels(cholera)

#exporting data

rio::export(cholera, here("Data/Cholera/cholera_cleaned.csv"))



#data visulalization----

cholera <- cholera %>% 
crosstable(cholera, c(age_yr))
crosstable(cholera, c(age_yr)) %>% flextable::as_flextable() %>% 
  crosstable(cholera, c(sex))
crosstable(cholera, c(sex), c(age_cat), by = c(country), total = "both") %>% 
  flextable::as_flextable()

#plotting
cholera %>% 
  ggplot (aes (x = age_cat, fill = sex)) +
  geom_bar () + labs(title = "Age category vs. sex", x =
                       "Age category", y = "Count") + theme_minimal()

# map

library(tidyverse)
map_emr <- map_data('world')[map_data('world')$region %in% c("Egypt"),]

ggplot() +                 # first layer
geom_polygon(data = map_data("world"), 
             aes(x=long, y=lat, group = group), 
             color = 'blue', fill = 'lightblue')+ 
  
  geom_polygon(data = map_emr,
               aes(x=long, y=lat, group = group),
               color = 'red', fill = 'pink')


# daily epi-curve



cholera <- cholera %>% 
daily_epicurve2 <- incidence(dates = cholera$adm_date3, interval = "day")
plot(daily_epicurve)


weekly_epicurve <- incidence(dates = cholera_2023$adm_date2, interval = "Sunday week")
plot(daily_epicurve)


#ggplot

daily_epicurve2 <- cholera %>% 
  ggplot(aes(adm_date2))+
  geom_histogram(binwidth = 1)
daily_epicurve2


