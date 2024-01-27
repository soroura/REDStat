# Any line that starts with the character '#' is a 'comment', and is ignored
# when you run the code. You can use this fact to annotate your code, as I am
# doing right now.

# R files conventionally start with a bunch of library(...) statements
# This loads up packages that other people have written for certain tasks
# For example, tidyverse provides a set of commands for manipulating data
# that is used ubiquitously in R.
# NHANES stands for National Health and Nutrition Examination Survey
# They collect health and nutrition data of people in the USA
# https://www.cdc.gov/nchs/nhanes/index.html
# BSDA is a basic statistics package
# There is an R package associated with NHANES that has a sample dataset that
# we use throughout this course.

# Install required packages
install.packages('tidyverse')
install.packages('NHANES')
install.packages('BSDA')

# Load required packages
library(tidyverse)
library(NHANES)
library(BSDA)

##### Load in the NHANES sample dataset
data(NHANES)

# After running this command, an object called NHANES should appear in
# the top right hand tab. Click on it and have a look what it consists of.
# Use your favourite exploration functions (eg. head() or str())
# to find out a few things about this dataset.

##### Z-test

# This code does a z-test for the hypothesis that the mean number of hours of 
# sleep each night in the population is 7, assuming that the population standard
# deviation is 1.5

z.test(NHANES$SleepHrsNight, mu = 7, sigma.x = 1.5)

### Question
# What p-value did you get? How would you interpret it?
### Answer
# The p-value is very small. We are very unlikely to have seen this result
# under the null hypothesis that mean hours of sleep each night is 7 hours.




##### Chi-squared test

# Here, we will focus on the Gender and Education variables from the NHANES dataset
# Are men's and women's education levels distributed in the same way?
# Let's run a Chi-squared test to find out.

# Create a contingency table with gender as row variable, and level of education
# as the column variable.
# Note that in creating the contingency table, we use the table() function that
# you're already familiar with, and then we use the as.data.frame.matrix() command
# to turn the table into a dataframe, which is easier to view and manipulate than a simple table.
contingency_table <- as.data.frame.matrix(table(NHANES$Gender, NHANES$Education))

# After you run the last line, an object called contingency_table will appear
# in the top right hand tab. Click on it. Is it what you expected?

# This next line of code does a chi-squared test of whether the proportions of
# men and women with each level of education are equal.
chisq.test(contingency_table)

### Question?
# What did you learn from this test?


##### One-sample t-test

# This code does a one-sample t-test for the null hypothesis that average height
# in the population is equal to 162cm
t.test(NHANES$Height, mu = 162)

# What is your interpretation of the results of this test?


##### Two-sample t-test

# We will do a t- test that the mean height of men and women in the population
# is equal. Do you expect a high or low p-value for this test?

# Select the heights of the men
# Use the 'pull' function to put those values into a vector
# The pull function is equivalent to using the $ sign, but is more
# tidyverse-friendly.
male_heights <- NHANES %>% filter(Gender == 'male') %>% pull(Height)

# Select the heights of the women
# Use the 'pull' function to put those values into a vector
female_heights <- NHANES %>% filter(Gender == 'female') %>% pull(Height)

# Do a two-sample t-test
t.test(male_heights, female_heights)

# Alternatively, you can use the formula notation:
# In this case, you don't need vectors.
# You can simply provide the dependent variable (here: Height)
# and the name of the variable that has the 2 groups
# that you want to compare (here: Gender)
# You also need to provide the name of the data object (here: NHANES)
t.test(Height ~ Gender, data = NHANES)

# How would you interpret the result of this test?
# Is it what you expected?




