
###############################################################################
# Regional training on R software
# Amman - Jordan - 3-7 Dec 2023
###############################################################################

# About this script -------------------------------------------------------

# Day 3 - PART II: Creating tables to summarize data & Data Visulization
# Author: Ronaldo Silva
# Purpose: Creating tables to summarize data & Data Visulization
# Date: 2023-10-10
# Version: 1.0



# Install/load packages ---------------------------------------------------

pacman::p_load(
  data.table,             # fast to load large datasets
  ggplot2,                # data visualization
  crosstable,             # generate tables
  gtsummary,              # generate tables
  flextable,              # enhance table visualization
  dplyr,                  # data processing and manipulation
  Hmisc,                  # label variables
  epikit,                 # age categories
  maps,                   # draw geographical maps
  mapproj,                # draw geographical maps
  lubridate,              # date manipulation
  incidence,               # epidemiological curve,
  janitor,                # data cleaning + quick tables
  skimr,
  tidyverse,               # data management
  aweek                  # date to week
)


# load dataset ------------------------------------------------------------
path <- "place your file path here and do not forget to change slash direction"
cholera <- fread(file = paste0(path,"Data/Cholera case study/cholera_20231102.csv"), header = TRUE)


# Data Processing from DAY 2 HERE! ------------------------------------------
cholera <- cholera %>%     # piping cholera dataframe through all processing steps
  # clean names of all columns in a one go (check other possible arguments) 
  clean_names() %>% 
  rename(                  # manual renaming (!! new name = old name)
    case_cat = f_diag
  ) 

cholera <- cholera %>% 
  # modifying same column or creating new one using mutate
  mutate( sex= recode(sex,        
                      "f" = "female",
                      "F" = "female",
                      "Female" = "female",
                      "M" = "male",
                      "m" = "male",
                      "Male" = "male"
  ))
  
  
cholera <- cholera %>%   mutate(
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
                            by= 10)
)

cholera <- cholera %>% mutate(bmi = round(wt_kg/(ht_cm/100)^2))
   

#drop blank columns
cholera <- cholera %>% select(-c(muac, ocv_doses, starts_with(c("rdt_", "cult_"))))

label(cholera$bmi)="kg/m2"


# Data Processing from DAY 3 - PART 1 HERE! ---------------------------------

### convert to date class ----
cholera <- cholera %>% 
  # converting character to date
  mutate(#adm_date= as.Date(adm_date, format = "%m-%d-%Y"),
    adm_date= mdy(adm_date),
    # converting numeric to date
    outcome_date= as.Date(outcome_date, origin= "1899-12-30"),
    onset_date = ymd(onset_date)
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



# ****************************** DAY 3 PARTE 2 *********************************#


# 1. Person --------------------------------------------------------------------

## Demographics ------------
## Age
crosstable(cholera, c(age_yr))
crosstable(cholera, c(age_yr)) %>% flextable::as_flextable()

# plotting the boxplot for age_yr
ggplot(cholera, aes(y = age_yr)) + # adding new step in ggplot with (+) sign  
  geom_boxplot() +  geom_jitter(aes(x = 0), width = 0.1, size = 1, color = "black") + # this is to add the distribution of data value  
  labs(title = "Boxplot of Age", y = "Age") 
  
#  Age in categories
crosstable(cholera, c(age_cat)) %>% flextable::as_flextable()

# plotting age categories 
cholera %>%
  ggplot(aes(x = age_cat)) + 
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(title = "Barplot of Age Categories", y = "Count", x = "Age Category") +
  theme_minimal()

## Sex - using crosstable or tbl_summary from gtsummary package
#crosstable(cholera, c(sex_f)) %>% flextable::as_flextable()
cholera %>% 
  dplyr::select(sex_f) %>%  # you need to select the variable included in the table if using gtsummary
  tbl_summary() 


## cross-tabulation of Age by sex
crosstable(cholera, c(age_cat2), by=c(sex_f)) %>% flextable::as_flextable()

## **Assignment** -> Create a table by Age in categories by sex

## plotting age categories by sex using barplot
cholera %>%
  ggplot(aes(x = age_cat2, fill = sex_f)) + 
  geom_bar(position = "dodge") +
  labs(
    title = "Barplot of Age Categories by Sex", 
    y = "Count", 
    x = "Age Category",
    fill = "Sex at birth") +
  theme_minimal() +
  scale_fill_manual(values = c(Male="lightgreen", Female="lightblue")) +
  coord_flip()

## plotting age year by sex using barplot 
plot_age_sex <- cholera %>%
  ggplot(aes(x=sex_f, y = age_yr, fill=sex_f)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.1), size = 1, color = "black") +
  labs(title = "Boxplot of Age by Sex", y = "Age", x = "Sex") 

ggsave("C:/Users/rosilva/OneDrive - World Health Organization/Documents/WHO_projects/Capacity building/EMRO/R Training Amam 2023/Day 3/plot_age_sex.png",width = 12, height = 7)


## Clinical Characteristics ------------
# labels 1 ------------------------------------------------------------------
label(cholera$sym_conf) = "Confusion/irritability"
label(cholera$sym_sob) = "Shortness of breath"
label(cholera$sym_weak) = "Weakness"
label(cholera$sym_myalg) = "Myalgia"
label(cholera$sym_abd_pain) = "Abdominal Pain"
label(cholera$sym_anorex) = "Anorexia"
label(cholera$sym_naus) = "Nausea"
label(cholera$sym_diar) = "Diarrhoea"
label(cholera$sym_vomit) = "Vomiting"

label(cholera$com_hep) = "Hepatitis"
label(cholera$com_diab) = "Diabetes"
label(cholera$com_hiv) = "HIV"
label(cholera$com_ckd) = "Chronic kidney disease"
label(cholera$com_chd) = "Chronic heart failure (include congenital)"
label(cholera$com_cpd) = "Chronic pulmonary disease"
label(cholera$com_cld) = "Chronic liver disease"


# Symptoms ----

# symptoms is a vector of symptoms in your dataset using c() function - this faciltates when you need to tabulate lots of variables
symptoms <- c("sym_conf", "sym_sob", "sym_weak", "sym_myalg", "sym_abd_pain", "sym_anorex", "sym_naus", "sym_diar", "sym_vomit")
crosstable(cholera, c(symptoms),total="both", percent_pattern="{n} (c%={p_col}))") %>% flextable::as_flextable()
crosstable(cholera, c(symptoms), by=c(sex_f), total="both", percent_pattern="{n} (c%={p_col})\n (r%={p_row}))") %>% flextable::as_flextable()

# for sympatoms by age, we will save the output using write.csv2; the created tab_cond_age.csv can then be used in othe software
tab_cond_age <- crosstable(cholera, c(symptoms), by=c(age_cat), total="both", percent_pattern="{n}") 
write.csv2(tab_cond_age, "place the file path here/tab_cond_age.csv")
write.csv2(tab_cond_age, "place the file path here/tab_cond_age_b.csv")

# Underlying conditions
conditions <- c("com_hep", "com_diab", "com_hiv", "com_ckd", "com_chd", "com_cpd", "com_cld")
crosstable(cholera, c(conditions),total="both", percent_pattern="{n} (c%={p_col}))") %>% flextable::as_flextable()
crosstable(cholera, c(conditions), by=c(age_cat), total="both", percent_pattern="{n} (c%={p_col})\n (r%={p_row}))") %>% flextable::as_flextable()

## Final Diagnosis
crosstable(cholera, c(case_cat)) %>% flextable::as_flextable()

## Admission Date
crosstable(cholera, c(adm_date)) %>% flextable::as_flextable()

## Discharge Date
crosstable(cholera, c(outcome_date)) %>% flextable::as_flextable()

## Length of Stay
crosstable(cholera, c(los)) %>% flextable::as_flextable()

ggplot(cholera, aes(x = los)) +
  geom_histogram(position = "dodge", binwidth = 1) + # position="dodge" makes the bars side-by-side
  labs(x = "Length of Stay (days)", y = "Count", fill = "Outcome", title = "Histogram of Length of Stay by Outcome") +
  theme_minimal()

## Symptom Onset
crosstable(cholera, c(onset_date)) %>% flextable::as_flextable()

# Outcome at discharge
crosstable(cholera, c(outcome)) %>% flextable::as_flextable()



# Epidemiology -----------------------------------------------------------------

# 2. Place -------- 
## Country and Zone
crosstable(cholera, c(hospital), by=c(country), total="both", percent_pattern="{n} (c%={p_col})\n (r%={p_row}))") %>% flextable::as_flextable()


# Map ----
map_emr <- map_data('world')[map_data('world')$region %in% c("Jordan","Tunisia","Egypt"),]

ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data("world"),
               aes(x=long, y=lat, group = group),
               color = '#9c9c9c', fill = '#f3f3f3') +
  ## Second layer: Country map
  geom_polygon(data = map_emr,
               aes(x=long, y=lat, group = group),
               color = 'red', fill = 'pink') +
  coord_map() +
  ggtitle("A map of Cholera") +
  theme(panel.background =element_rect(fill = 'lightblue'))





