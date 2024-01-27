### Week 2 lab - Checking the normality assumption

# In this self-guided lab, you will practice checking the assumption of normality in R using 
# graphs and hypothesis tests.

# We'll be using functions from the tidyverse library, so let's load it in too.
library(tidyverse)

## Exercise 1
# Using the NHANES dataset, imagine you want to investigate the difference in average diastolic
# blood pressure (BPDiaAve) between adult males and females. The exercise here is to check 
# whether the normality assumption holds for each subgroup, using both graphs and hypothesis 
# tests.


# We will be looking at the NHANES dataset, a survey dataset collected by the US National 
# Center for Health Statistics (NCHS) which has conducted a series of health and nutrition 
# surveys since the early 1960's. Since 1999, approximately 5,000 individuals of all ages are 
# interviewed in their homes every year and complete the health examination component of the
# survey. Find out more here: https://cran.rstudio.com/web/packages/NHANES/index.html, 
# and check the reference manual for the R package:
# https://cran.rstudio.com/web/packages/NHANES/NHANES.pdf

# Let's start by installing the NHANES package and loading it into R
install.packages("NHANES")
library(NHANES)

# Now let's tell R that we'll be using the NHANES data
data("NHANES")

# And let's have a quick look at the dataset
str(NHANES)
head(NHANES)

# You'll notice that there are 76 variables and 10,000 observations in this dataset.
# The variables are a mix of numerical and categorical variables. For this exercise,
# we will focus on the numerical average diastolic blood pressure variable (BPDiaAve) and 
# the categorical Gender variable.
# Remember to filter the dataset based on the numerical Age variable to keep only adult 
# individuals (aged 18 or over), and based on the Gender to look at each subgroup separately.

## Your solution below:


## Exercise 2
# This time, we will use a simulated dataset of birthweights, and we will check again whether
# the normality assumption holds.

# First we create a dataset (tibble) of 1000 birth weights with a mean of 3510 grams and 
# a standard deviation of 385 grams.

birthweight <- tibble(
  birthwt = rnorm(1000,3510,385)
)

# This creates a tibble named birthweight with one numerical variable named birthwt and
# 1000 observations. Does the birthwt variable have a normal distribution?

## Your solution below:


## This is the end of the lab!
## Now you're ready to complete the Week 2: 3 stars and a wish on the Discussion Board.