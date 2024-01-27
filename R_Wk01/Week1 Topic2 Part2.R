## Only install the libraries below when you run this script for the first time.

# install.packages("NHANES") # data package
library(tidyverse)
library(NHANES)

# Let's load the NHANES data
data(NHANES)

# Let's use R to calculate measures of central tendency in the Pulse variable

# Mean
# Remember from the frequency table that we do have missing values
# We need to tell R explicitly to remove them from the mean calculation
# Otherwise we will get NA as the outcome
mean_pulse <- mean(NHANES$Pulse, na.rm = TRUE)
mean_pulse

# Median
median_pulse <- median(NHANES$Pulse, na.rm = TRUE)
median_pulse

# Mode
# There is no ready-made function in R to calculate the mode
# We can write our own function (optional)
# This function comes from StackOverflow:
# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
# Understanding the details of it is beyond the scope of this course
# But if you are curious about it, please feel free to ask Kasia 

calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  # this line creates a vector of unique values of x (in our case, Pulse)
  uniqx[which.max(tabulate(match(x, uniqx)))]
  # this line creates a table of the frequency of each unique value
  # and then picks the value with the highest frequency
}

mode_pulse <- calculate_mode(NHANES$Pulse)
mode_pulse
