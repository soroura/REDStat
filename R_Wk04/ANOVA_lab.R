# Week 4 lab - ANOVA with the NHANES dataset

### Introduction ###

# In this self-guided lab, you will be running an ANOVA. We will be using
# a significance level of 5% throughout.

# We'll be using functions from the tidyverse collection of packages, as
# well as from the emmeans package, and we will use the NHANES dataset
# again, so let's load them in.

library(tidyverse)
library(emmeans)
library(NHANES)

# The messages above are telling us which functions from the base R are
# overwritten by functions from the Tidyverse, and that some packages were
# set up using a previous version of R.

### ANOVA exercise ###

# Let's explore whether people who watch more TV also tend to have a
# higher body mass index (BMI), indicating that they are more likely to be
# overweight or obese. We will use a one-way ANOVA to answer this
# question.

# In order to meet the assumptions for ANOVA, we will restrict the NHANES
# dataset a little bit. This is just for demonstration purposes, if you
# were given a real dataset, you would need to analyse the whole dataset,
# rather than restricting it.

# Let's create a new dataset, called NHANES_ANOVA, where we will create a
# new version of the variable about watching TV - my TV_categorical
# variable will only have 3 levels, and it will indicate whether someone
# watches TV less than the median (i.e.less than 2 hours a day), at the
# median (2 hours a day), or more than the median (3 or more hours). We
# will also filter out people with BMI over 40 (again, this is for
# demonstration purposes only).

NHANES_ANOVA <- NHANES %>% 
  # creating a TV_categorical variable with 3 levels
  mutate(TV_categorical = case_when(TVHrsDay == "0_hrs"|TVHrsDay == "0_to_1_hr"|
                                      TVHrsDay == "1_hr" ~ "less_than_median",
                                    TVHrsDay == "2_hr" ~ "median",
                                    TVHrsDay == "3_hr"|TVHrsDay == "4_hr"|
                                      TVHrsDay == "More_4_hr"
                                    ~ "more_than_median")) %>% 
  # removing people with a missing value for TV_categorical
  drop_na(TV_categorical) %>%
  # filtering out people with BMI of 40 or more
  filter(BMI < 40)

# Now we will have a quick look at our NHANES_ANOVA subset to see how many
# people there are in each category.

table(NHANES_ANOVA$TV_categorical)

## Task 1: Dataset exploration.

# Let's create a box plot and a table of summary statistics including mean
# and standard deviation to see what the pattern is for BMI across
# TV_categorical groups. Based on the descriptive statistics and the
# boxplot, what pattern do you see in the data?

## Your solution


## Task 2: ANOVA

# Let's run an ANOVA to test if what we're seeing is statistically
# significant. How would you interpret the results of the ANOVA?

## Your solution


## Task 3: Post-hoc tests

# As you know, ANOVA is an omnibus test, so it doesn't tell us where the
# significant differences lie. We need to look at post-hoc comparisons to
# tell us which groups are different. Let's do it using Tukey's HSD test
# for each pairwise comparison. We would like to get the differences in
# means, their test statistic and associated p-value, standard error and
# confidence interval. How would you interpret the results of this test?

## Your solution


## Task 4: Checking the assumptions.

# Let's check the assumptions.

#-   4.1. Independence. Would you say that the data meet the independence
#    assumption? Are the groupings independent of one another? Is each
#    observation independent of the others?

#-   4.2. Homogeneity of variance. Does the BMI variable have a similar
#    variance among the three groups? Run a Bartlett's test to check. How
#    would you interpret the results of this test?
  
#-   4.3. Normality. Does the BMI variable come from a normal
#    distribution? Use both visual inspection of the distribution and
#    hypothesis test. How would you interpret the plot and the test
#    results?

## Your solution

