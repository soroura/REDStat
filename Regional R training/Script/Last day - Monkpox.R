# About ----
#'Name: last day excercise
#'Author: Hossam Hassan
#'Date: 07/12/2023
#'

# Load packages ----

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

# Data Source----

Mpox <- import(here("Mpox/mpox_20231012.csv"))


# Data exploring----
str(Mpox)

# Data processing----
names(Mpox)

mpox0 <- Mpox %>% janitor::clean_names()

mpox0 <- mpox0 %>% dplyr::mutate(age_cat = age_categories(age_yr,
                                                          lower = 0,
                                                          upper = 120,
                                                          by = 15))

mpox0 <- mpox0 %>% dplyr::mutate(symp_onset2 = as.Date(symp_onset))
mpox0 <- mpox0 %>% dplyr::mutate(adm_date2 = as.Date(adm_date))
mpox0 <- mpox0 %>% dplyr::mutate(spec_date2 = as.Date(spec_date))
mpox0 <- mpox0 %>% dplyr::mutate(symp_onset2 = as.Date(symp_onset))

mpox0 <- mpox0 %>% dplyr::mutate(epiweek = aweek::date2week(adm_date2, start_week = 7, floor_day = 1))

#Symptoms to admission

