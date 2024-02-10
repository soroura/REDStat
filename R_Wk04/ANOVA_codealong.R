# Installing packages - uncomment the lines below if you need to install.
# install.packages("NHANES")
# install.packages("tidyverse")
# install.packages("emmeans")
# install.packages("car")

# Loading libraries
library(NHANES) # for the dataset
library(tidyverse) # for wrangling and plotting
library(emmeans) # for post-hoc tests
library(car) # for Levene's test

# NULL HYPOTHESIS: Working status doesn't affect the number of hours of sleep per night

summary(NHANES) # to see descriptive statistics of all variables

# We will keep only adults (aged 18 and over)
# and three variables: HealthGen, SleepHrsNight, and Work
dataset1 <- NHANES %>% 
  filter(Age > 18) %>%
  select(HealthGen, SleepHrsNight, Work)

summary(dataset1)
# we see that there are three categories for the Work variable
# this shows that Work has 1 missing value (NA) and SleepHrsNight has 17
head(dataset1)
# We see that the Work variable is a factor
# How many levels (or categories) does Work have?
# Using base R
levels(dataset1$Work)

# How many observations in each Work group?
# Using base R
table(dataset1$Work)

# Using the Tidyverse
dataset1 %>%
  pull(Work) %>% # extract Work as a vector (of factor type)
  fct_count()

# As seen in the summary() output, Work has three categories:
# Looking, NotWorking and Working

# Check the general structure of our dataset1
str(dataset1)

# Calculate the mean number of hours slept at night for each Work category
# The group_by() function will run subsequent functions separately for each
# category of the Work variable.
dataset1 %>%
  group_by(Work) %>%
  summarise(mean=mean(SleepHrsNight))
# The NA results are caused by missing values for SleepHrsNight in all three 
# Work categories

# So we will update dataset1 by removing the rows with missing Work or SleepHrsNight data
# Note that there are many other ways to deal with missing data, not covered here
# If we don't remove missing data at the start, we will need to add the na.rm=T
# argument to functions like mean(), sd(), var(), etc.

# Add the na.rm = TRUE argument in the mean function to remove only missing data 
# within that function call.
dataset1 %>% 
  group_by(Work) %>% 
  summarise(mean = mean(SleepHrsNight, na.rm = TRUE))

# Update dataset1 to remove all rows with missing data for either SleepHrsNight or Work
dataset1 <- dataset1 %>% 
  drop_na(SleepHrsNight,Work)

# Now we can calculate the mean, standard deviation and variance for SleepHrsNight
# for each Work category
sleep_descriptives_by_workingstatus <- dataset1 %>%
  group_by(Work) %>%
  summarise(mean=mean(SleepHrsNight),
            sd = sd(SleepHrsNight),
            var = var(SleepHrsNight))
# View the summary statistics tibble we just created
sleep_descriptives_by_workingstatus
# This table suggests that Working individuals have slightly less sleep than others,
# especially when compared with NotWorking individuals.

# Boxplot to visualise differences before ANOVA
dataset1 %>%
# colour each boxplot by working status with the fill argument
  ggplot(aes(x = Work, y = SleepHrsNight, fill = Work)) +
  geom_boxplot() +
  # add a title for the plot
  labs(title = "Figure 1: Boxplot of Hours of Sleep by Work situation")

# The boxplots seem very similar, symmetrical around the centre, and 
# with similar centre and variability across categories,
# but large sample sizes might bring small differences out.

# Perform the ANOVA
ANOVA1 <- aov(SleepHrsNight~Work, data=dataset1)
# This doesn’t give any output, for that you need to use the summary() function.
# The summary() function will create a table with one row for each categorical 
# variable and each interaction term, if you have any.
summary(ANOVA1)
# There is one row for the Work variable, which shows the among group information,
# and one row for the residuals.
# For each row, there is the number of degrees of freedom, the sum of squares,
# and the mean square
# For the Work variable (treatment, or explanatory variable), there is also 
# the F-value and the associated p-value.
# Here the df for Work is 3 (Work categories) - 1 = 2
# df for the residuals = 7354 observations - Work categories = 7351
# Work Sum of squares = differences among Work categories
# Residual Sum of squares = variation within Work categories
# Mean Square = SS / df
# F = MS among / MS within = 48.99 / 1.77
# Small p-value: strong evidence that Work category influences the number of 
# hours of sleep at night

# POST-HOC TESTS
# Evidence for differences among Work categories
# Compare means among Work categories
anova1_emmeans <- emmeans(ANOVA1,"Work")
anova1_emmeans
# This outputs a table with a row for each Work category
# It gives a mean, standard error, number of df,
# lower and upper boundaries of the confidence intervals for
# these means. The standard error uses the standard deviation
# of the residuals estimated from the model
# SE = SD / sqrt(n)
SD_residuals<-sigma(ANOVA1)
n_looking<-dataset1 %>%
  group_by(Work) %>%
  count(Work) %>%
  filter(Work=="Looking") %>%
  ungroup() %>%
  select(n)
SE_looking<-SD_residuals/sqrt(n_looking) # 0.0801

# Pairwise comparisons
anova1_pairs <- pairs(anova1_emmeans)
anova1_pairs
# estimate = effect size, difference between means for pairs of Work categories
# e.g. Looking - NotWorking = 7-7.04 = -0.04
# SE = standard error for this difference / effect size
# t.ratio and p.value are t-statistic and p-value for Tukey's 
# test with H0: the difference between the means = 0
# Highly significant for NotWorking vs Working
# Tukey's "Honest Significant Differences" test 
# corrects for multiple pairwise tests,
# therefore no increase of type 1 error

# Similar output, with 95% CI, also corrected for multiple
# testing, instead of t-statistic and p-value
confint(anova1_pairs)
# Largest difference (NotWorking vs Working) has a positive
# adjusted 95% CI, doesn't overlap 0

# Plot confidence intervals
plot(confint(anova1_pairs))

# ASSUMPTIONS
# 1) Independent data
# 2) Normality
# 3) Homogeneity of variance

# 1) Independent data

# As we've seen before, each row of the NHANES dataset should be a 
# different individual, therefore the assumption of independence is met.

# 2) Normality

# Histogram
dataset1 %>% 
  ggplot(aes(x = SleepHrsNight)) +
  geom_histogram(bins = 10) +
  facet_wrap(facets = ~Work, ncol = 1, scales = "free_y")

# Q-Q plot to assess normal distribution of SleepHrsNight
ggplot(dataset1, aes(sample = SleepHrsNight)) +
  stat_qq() + # this line plots the error terms
  stat_qq_line() # this line plots the regression line
# The histograms and Q-Q plot suggest that SleepHrsNight comes from a normal
# distribution.

# Shapiro-Wilk normality test
# shapiro.test(dataset1$SleepHrsNight)
# The error message says we can't have more than 5000 observations.

# We create a random sample of dataset1 with just 4000 observations.
sample <- slice_sample(dataset1, n = 4000)
shapiro.test(sample$SleepHrsNight)
# Significant departure from normality, maybe due to large sample.

# 3) Homogeneity of variance

# Boxplot as before
dataset1 %>% 
  ggplot(aes(x = Work, y = SleepHrsNight, fill = Work)) +
  geom_boxplot() +
  labs(title = "Figure 1: Boxplot of Hours of Sleep by Work situation") 

# The boxplots suggest that there is equal variance  of
# SleepHrsNight between Work categories
# Can also use Bartlett's test of homogeneity of variances
# H0: variances are equal across groups
bartlett.test(dataset1$SleepHrsNight~dataset1$Work)
# Or Levene's test if the variable is not normally distributed
# Levene's test with one independent variable
leveneTest(SleepHrsNight ~ Work, data = dataset1)

## BONUS PLOT
## Residual plot to see if the residuals are similarly distributed across groups.

# Create a tibble with the fitted values, residuals and Work category
Model <- tibble(Fitted = fitted(ANOVA1), # extracts fitted values from ANOVA1
                Residuals = resid(ANOVA1), # extracts residuals from ANOVA1
                Work = dataset1$Work) # takes the Work categories from dataset1

# Create a residual plot for the ANOVA model, which shows the residuals for 
# each Work category The residuals on the y-axis are the differences between 
# the actual data points and the fitted values. The fitted values on the x-axis
# are the means for each treatment. We expect residuals to have a mean of 0 if
# there is a symmetric distribution of SleepHrsNight for each Work category.
# The residuals shouldn't show any pattern and be roughly evenly distributed 
# above and below 0.
ggplot(Model, aes(Fitted, Residuals, colour = Work)) +
  geom_point()