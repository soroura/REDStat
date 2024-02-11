##############################################################################
# Regional training on R software
# Amman - Jordan - 3-7 Dec 2023
###############################################################################

# About this script -------------------------------------------------------

# Day 1: Packages and functions
# Author: Basma AbdElGawad
# Purpose: Install and load packages for training purposes + exercise solution 
# Date: 2023-11-19
# Version: 1.0



# Load packages -----------------------------------------------------------

# you may install the package using install.packages function
#loading outbreaks package
library(outbreaks)

# Install/load packages using pacman package

pacman::p_load(
  data.table,             # fast to load large datasets
  rio,                    # to import/export different types of data
  here,                   # set relative path to project root
  
  dplyr,                  # data processing and manipulation
  lubridate,              # date manipulation
  Hmisc,                  # label variables
  janitor,                # data cleaning + quick tables
  epikit,                 # age categories
  stringr,                # working with string variables
  
  crosstable,             # generate tables
  gtsummary,              # generate tables
  flextable,              # enhance table visualization
  
  ggplot2,                # data visualization
  scales,
  incidence,              # epidemiological curve
  
  tidyverse               # data management
)



# Exercise solution -------------------------------------------------------

## assign dataset to an object ----
cases <- outbreaks::ebola_sierraleone_2014

## exploring dataset -----

str(cases)
summary(cases)
class(cases)

class(cases$date_of_onset)

mean(cases$age, na.rm = TRUE) # na.rm is a parameter used to handle missings in the data
median(cases$age, na.rm = T)
range(cases$age, na.rm = TRUE)
summary(cases$age)

# earliest date of onset
min(cases$date_of_onset, na.rm = TRUE)
max(cases$date_of_sample, na.rm = T)
