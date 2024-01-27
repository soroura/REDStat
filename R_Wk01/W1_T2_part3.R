## Only install the libraries below when you run this script for the first time.

# install.packages("NHANES") # data package
library(tidyverse)
library(NHANES)

# Let's load the NHANES data
data(NHANES)

# Standard deviation
sd_pulse <- sd(NHANES$Pulse, na.rm = TRUE)
sd_pulse

# Interquartile range
iqr_pulse <- IQR(NHANES$Pulse, na.rm = TRUE)
iqr_pulse
