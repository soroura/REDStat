### Week 2 lab - Checking the normality assumption

# In this self-guided lab, you will practice checking the assumption of normality in R using 
# graphs and hypothesis tests.

# We'll be using functions from the tidyverse library, so let's load it in too.
library(tidyverse)

## Exercise 1  ----
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
subsetNHANES <- NHANES %>% 
  filter(Age >= 18) %>% 
  drop_na(BPDiaAve, Gender) 

# Then we can check the assumption of normal distribution of the BPDiaAve
# variable. We can plot a histogram for BPDiaAve for each Gender.

subsetNHANES %>% 
  ggplot(aes(x= BPDiaAve)) +
  geom_histogram(col="black", fill="lightblue", bins = 30) +
  facet_wrap(~Gender) +
  theme_bw() +
  theme(panel.grid = element_blank())

# The histograms look close to normal, but there is a slightly longer left tail,
# suggesting that the data has a negative (or left-) skew. We can also plot a
# Normal Q-Q plot for BPDiaAve for each Gender.

subsetNHANES %>%
  ggplot(aes(sample=BPDiaAve)) +
  stat_qq() +
  stat_qq_line(color=2) +
  facet_wrap(~Gender) + ## plot Q-Q plots for each gender side-by-side
  labs(title="Normal Q-Q Plot") + ## add title
  theme_bw() + ## remove gray background
  theme(panel.grid=element_blank()) ## remove grid

# The QQ-plots confirm the observations from the histogram and also show a
# deviation from normality on the right-hand side of the plot.  

# Moving on to hypothesis tests, we perform the Shapiro-Wilk test on the
# BPDiaAve variable for each Gender 

# Males

subsetNHANES %>%
  filter(Gender=="male") %>% ## keep only male individuals
  pull(BPDiaAve) %>%
  shapiro.test()

# Females
subsetNHANES %>%
  filter(Gender=="female") %>% ## keep only female individuals
  pull(BPDiaAve) %>%
  shapiro.test()

# We can also use the Kolmogorov-Smirnov test BPDiaAve to test for normality.

# Males
subsetNHANES %>%
  filter(Gender=="male") %>% ## keep only male individuals
  pull(BPDiaAve) %>%
  ks.test(.,"pnorm",mean=mean(.),sd=sd(.))

# Females
subsetNHANES %>%
  filter(Gender=="female") %>% ## keep only female individuals
  pull(BPDiaAve) %>%
  ks.test(.,"pnorm",mean=mean(.),sd=sd(.))

# Both hypothesis tests show a significant departure from normality, although we
# should keep in mind that they are sensitive to small departures, especially
# when looking at a large dataset such as NHANES. The warning for the
# Kolmogorov-Smirnov test means that there are some ties in the ranks of the
# data. 

# Overall, it seems like the distribution of the average diastolic blood
# pressure is not quite normally distributed in either of the two Gender categories.


## Exercise 2 ----
# This time, we will use a simulated dataset of birthweights, and we will check again whether
# the normality assumption holds.

# First we create a dataset (tibble) of 1000 birth weights with a mean of 3510 grams and 
# a standard deviation of 385 grams.

birthweight <- tibble(
  birthwt = rnorm(1000,3510,385)
)

# This creates a tibble named birthweight with one numerical variable named birthwt and
# 1000 observations. TASK:   Does the birthwt variable have a normal distribution?

## Your solution below:

# We first plot a histogram of brithwt variable. 
birthweight %>%
  ggplot(aes(x = birthwt)) +
  geom_histogram(col="black",fill="lightblue",bins = 35 ) +
  theme_bw() + ## remove gray background
  theme(panel.grid=element_blank()) ## remove grid

# The histogram seems close to the bell shape expected for a normal
# distribution. We can also create a Normal Q-Q plot for birthwt.

birthweight %>%
  ggplot(aes(sample=birthwt)) +
  stat_qq() +
  stat_qq_line(color=2) +
  labs(title="Normal Q-Q Plot") + ## add title
  theme_bw() + ## remove gray background
  theme(panel.grid=element_blank()) ## remove grid

# The Q-Q plot also suggests that the data follows the normal quantiles for most
# of the data points, with a few data points at either end departing slightly
# from it.

# Moving on to hypothesis tests, we can perform the Shapiro-Wilk test on the birthwt variable.

birthweight %>%
  pull(birthwt) %>%
  shapiro.test()

# Finally, we can test for normality of birthwt using the Kolmogorov-Smirnov test.
birthweight %>%
  pull(birthwt) %>%
  ks.test(.,"pnorm",mean=mean(.),sd=sd(.))

# Both hypothesis tests give a p-value higher than 0.05, therefore we cannot
# reject the null hypothesis that the data is normally distributed. 

# Overall, it seems that the birthweight variable is normally distributed. You
# might have noticed that the function we used to create that data was actually
# sampling from a normal distribution, so we expected this to be normally
# distributed.

## This is the end of the lab!
## Now you're ready to complete the Week 2: 3 stars and a wish on the Discussion Board.