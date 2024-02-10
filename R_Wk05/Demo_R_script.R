# Demonstration of testing the association between two numerical variables
# and checking of assumptions for regression and t-tests

# Load the tidyverse package
library(tidyverse)

# Install the NHANES package with the dataset (only do this once!)
# install.packages("NHANES")

# Load the NHANES package
library(NHANES)

###############################################################################
# EXAMPLE 1: Investigate the relationship between BPDiaAve and BPSysAve in 
#            adult males
###############################################################################

# Create a subset of the NHANES dataset with only male individuals aged 18 and over,
# who have both BPDiaAve and BPSysAve information
subsetNHANES <-NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  drop_na(BPDiaAve,BPSysAve) %>% # remove rows where either BPDiaAve or BPSysAve is NA
  select(BPDiaAve,BPSysAve)

# Create a scatterplot for BPDiaAve (x axis) and BPSysAve (y axis)
subsetNHANES %>%
  ggplot(aes(x = BPDiaAve, y = BPSysAve)) + 
  geom_point(pch=20) + # plot the data points and change the dot shape / size
  xlab("Combined diastolic blood pressure reading (mm Hg)") + 
  ylab("Combined systolic blood pressure reading (mm Hg)") +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Calculate Pearson's correlation coefficient between BPDiaAve and BPSysAve
# and display the output as a tibble (table) with columns for the coefficient 
# estimate, t-statistic, p-value, degrees of freedom, lower and upper bound of 
# the 95% confidence interval
subsetNHANES %>%
  summarize(Pearsonr = cor.test(BPDiaAve,BPSysAve,method="pearson")$estimate,
            tstat = cor.test(BPDiaAve,BPSysAve,method="pearson")$statistic,
            pval = cor.test(BPDiaAve,BPSysAve,method="pearson")$p.value,
            df = cor.test(BPDiaAve,BPSysAve,method="pearson")$parameter,
            lowerCI = cor.test(BPDiaAve,BPSysAve,method="pearson")$conf.int[1],
            upperCI = cor.test(BPDiaAve,BPSysAve,method="pearson")$conf.int[2])

# Base R alternative to get the same information
with(subsetNHANES,cor.test(BPDiaAve,BPSysAve,method="pearson"))
cor.test(subsetNHANES$BPDiaAve,subsetNHANES$BPSysAve,method="pearson")

# Calculate Spearman's correlation coefficient between BPDiaAve and BPSysAve
# and display the output as a tibble (table) with columns for the coefficient 
# estimate, S-statistic, and p-value
subsetNHANES %>%
  summarize(Spearmanrho = cor.test(BPDiaAve,BPSysAve,method="spearman")$estimate,
            Sstat = cor.test(BPDiaAve,BPSysAve,method="spearman")$statistic,
            pval = cor.test(BPDiaAve,BPSysAve,method="spearman")$p.value)

# Base R alternative to get the same information
with(subsetNHANES,cor.test(BPDiaAve,BPSysAve,method="spearman"))
# You will get a warning here as there are too many ties for the ranks so R cannot
# compute an exact p-value

# Build a linear regression model to predict BPSysAve based on BPDiaAve
# using the subsetNHANES dataset and save the output as an object called lmodel
lmodel <- lm(BPSysAve ~ BPDiaAve, data = subsetNHANES)

# Display the coefficients (b0: intercept and b1: slope) for the model
lmodel$coefficients

# Display the 95% confidence intervals for each coefficient (beta 0 and beta 1)
confint(lmodel)

# Display a summary of the linear model, including: the variables and dataset
# used; summary statistics for the distribution of the residuals; the coefficient
# estimates and their associated standard error, t-value and p-value; model
# measure of quality (e.g. residual standard error, adjusted R-squared and F statistic)
summary(lmodel)

## The coefficients are significantly different from 0. But the model only explains
## 13% of the variances in the response variable BPSysAve.

# Using that linear regression model, predict the average systolic blood pressure
# value for an adult male with average diastolic blood pressure of 100 mm Hg
predict(lmodel, newdata = data.frame(BPDiaAve=c(100)),se.fit = T)


# Create the scatterplot, and this time add the regression line to it
subsetNHANES %>%
  ggplot(aes(x = BPDiaAve, y = BPSysAve)) + 
  geom_point() + 
  xlab("Combined diastolic blood pressure reading (mm Hg)") + 
  ylab("Combined systolic blood pressure reading (mm Hg)") +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_bw() +
  theme(panel.grid=element_blank())

###############################################################################
# EXAMPLE 2: Check the assumptions for a t-test comparing BPSysAve between 
#            Married and NeverMarried from the MaritalStatus variable for adult
#            males
###############################################################################

# Create a subset of the NHANES dataset with only male individuals aged 18 and over,
# who have BPSysAve information and MaritalStatus either Married or NeverMarried 
subsetNHANES <-NHANES %>%
  filter(Gender == "male"& Age >= 18 & 
           MaritalStatus %in% c("Married","NeverMarried")) %>%
  drop_na(BPSysAve) %>% # remove rows where BPSysAve is NA
  select(MaritalStatus,BPSysAve)

# 1. Assumption of normal distribution of variables

# Plot a histogram for BPSysAve for each marital status
subsetNHANES %>%
  ggplot(aes(x = BPSysAve)) + 
  geom_histogram(col="black",fill="lightblue",bins = 30 ) +
  facet_grid(~MaritalStatus) +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Create a Normal Q-Q plot for BPSysAve
subsetNHANES %>%
  ggplot(aes(sample=BPSysAve)) +
  stat_qq() +
  stat_qq_line(color=2) +
  facet_grid(~MaritalStatus) +
  labs(title="Normal Q-Q Plot") +    ## add title
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Perform the Shapiro-Wilk test on the BPSysAve variable for
# Married individuals
subsetNHANES %>%
  filter(MaritalStatus == "Married") %>%
  pull(BPSysAve) %>%
  shapiro.test()

# Perform the Shapiro-Wilk test on the BPSysAve variable for
# NeverMarried individuals
subsetNHANES %>%
  filter(MaritalStatus == "NeverMarried") %>%
  pull(BPSysAve) %>%
  shapiro.test()

# Test for normality of BPSysAve using the Kolmogorov-Smirnov 
# test for Married individuals
subsetNHANES %>%
  filter(MaritalStatus == "Married") %>%
  pull(BPSysAve) %>%
  ks.test(.,"pnorm",mean=mean(.),sd=sd(.))

# Test for normality of BPSysAve using the Kolmogorov-Smirnov 
# test for NeverMarried individuals
subsetNHANES %>%
  filter(MaritalStatus == "NeverMarried") %>%
  pull(BPSysAve) %>%
  ks.test(.,"pnorm",mean=mean(.),sd=sd(.))
# Here you will also get warnings about ties as the Kolmogorov-Smirnov test
# doesn't expect any ties in a continuous distribution

## The histograms suggest a distribution close to normal with a slight positive 
## skew, the Q-Q plots show a deviation on one end of the distribution for 
## each group.
## The Shapiro-Wilk and Kolmogorov-Smirnov tests are both very significant, but 
## that's expected for a large sample size (n=1969 for Married and n=724 for 
## NeverMarried).

# Optional
# Plot a histogram for log(BPSysAve) for each marital status
subsetNHANES %>%
  ggplot(aes(x = log(BPSysAve))) + 
  geom_histogram(col="black",fill="lightblue",bins = 30 ) +
  facet_grid(~MaritalStatus) +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# The histograms of the log-transformed variable seem closer to the normal distribution.

# 2. Homogeneity of variances
# If we had to compare the BPSysAve between Married and NeverMarried men in that
# dataset, we would check whether the variances were similar in both groups

# Visualise the variation for each group in a box plot
subsetNHANES %>%
  ggplot(., aes(x=MaritalStatus, y = BPSysAve)) +
  geom_boxplot(col="black",fill="lightblue") +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Display the standard deviations of both groups
subsetNHANES %>%
  group_by(MaritalStatus) %>%
  summarise(sd = sd(BPSysAve))

# Perform an F-test to test whether the variance in BPSysAve is similar for
# males and females
subsetNHANES %>%
  var.test(BPSysAve ~ MaritalStatus, ., alternative = "two.sided")

# You can run the non-parametric Levene's test of variances. For this you may
# need to install the car package if you don't already have it.
# install.packages("car")

# Load the car package
library("car")
subsetNHANES %>%
  leveneTest(BPSysAve ~ MaritalStatus, data = .)

## The box plots suggest that the spread is similar between the two groups.
## The tests of variances are significant, but quite close to the 5% significance
## level.


###############################################################################
# EXAMPLE 3: For adult males, check the assumptions of the linear regression
#            model predicting BPSysAve based on BPDiaAve (see Example 1).
###############################################################################

# We can use the lmodel object we created to predict BPSysAve based on BPDiaAve
# in Example 1.

# 1. Constant variability of residuals

# Produce a residual vs. fitted plot
ggplot(lmodel, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),color="red") +
  labs(y='Residuals',x='Fitted values') +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

## This plot suggests some non-random patterns of distribution of the residuals.
## It is likely that this assumption is not met.

# 2. Normal distribution of the residuals

# Get the list of residuals 
res <- resid(lmodel)

# Plot a histogram for the residuals
res %>%
  as_tibble() %>%
  ggplot(aes(x = res)) + 
  geom_histogram(col="black",fill="lightblue",bins = 30 ) +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Create a Normal Q-Q plot for the residuals
res %>%
  as_tibble() %>%
  ggplot(aes(sample=res)) +
  stat_qq() +
  stat_qq_line(color=2) +
  labs(title="Normal Q-Q Plot") +    ## add title
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Perform the Shapiro-Wilk test on the residuals
shapiro.test(res)

# Test for normality of the residuals using the Kolmogorov-Smirnov test
ks.test(res,"pnorm",mean=mean(res),sd=sd(res))

## The histogram seems "normal enough", but the Q-Q plot highlights a departure
## from the normal distribution at the ends of the range. Both normality tests
## suggest a departure from normality although we still have a large sample size
# (n=3581).

###############################################################################
# EXAMPLE 4: Investigate the correlation between BPSysAve and PhysActiveDays  #
#            for adult males.
###############################################################################

# You will notice that BPSysAve is numerical, whereas PhysActiveDays is discrete,
# and can be considered as categorical.
# There are two ways to go about this question. You can use the non-parametric
# Spearman's correlation method, or you can transform BPSysAve into a categorical
# variable and use a Chi-square test.

# Create a new subset of the data with only male individuals aged 18 and over,
# who have both PhysActiveDays and BPSysAve information
newsubsetNHANES <- NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  drop_na(PhysActiveDays,BPSysAve) # remove rows where either PhysActiveDays or BPSysAve is NA

# A. Spearman's correlation method

# Create a scatterplot for BPDiaAve (x axis) and BPSysAve (y axis)
newsubsetNHANES %>%
  ggplot(aes(x = PhysActiveDays, y = BPSysAve)) + 
  geom_point(pch=20) + # plot the data points and change the dot shape / size
  xlab("Number of days with physical activity") + 
  ylab("Combined systolic blood pressure reading (mm Hg)") +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Calculate Spearman's correlation coefficient between PhysActiveDays and BPSysAve
# and display the output as a tibble (table) with columns for the coefficient 
# estimate, S-statistic, and p-value
newsubsetNHANES %>%
  summarize(Spearmanrho = cor.test(PhysActiveDays,BPSysAve,method="spearman")$estimate,
            Sstat = cor.test(PhysActiveDays,BPSysAve,method="spearman")$statistic,
            pval = cor.test(PhysActiveDays,BPSysAve,method="spearman")$p.value)

# Base R alternative to get the same information
with(newsubsetNHANES,cor.test(PhysActiveDays,BPSysAve,method="spearman"))
# You will get a warning here as there are too many ties for the ranks so R cannot
# compute an exact p-value

# B. Transform BPSysAve into a categorical variable
newsubsetNHANES <- newsubsetNHANES %>%
  mutate(BPSysAveCat = case_when(BPSysAve <= 112 ~ 'low',
                                 BPSysAve > 112  & BPSysAve <= 129 ~ 'medium',
                                 BPSysAve > 129 ~ 'high')) %>%
  mutate(BPSysAveCat = as_factor(BPSysAveCat))

# Tabulate the PhysActiveDays and the new BPSysAveCat variables.
# This creates a contingency table with the number of observations (individuals),
# for each combination of categories of the two variables. It is essentially the
# observed values needed to calculate the chi-square statistic.
newsubsetNHANES %>% 
  count(BPSysAveCat,PhysActiveDays) %>% 
  spread(PhysActiveDays,n)

# Perform the chi-square test on these two variables
newsubsetNHANES %>%
  select(PhysActiveDays,BPSysAveCat) %>%
  table() %>%
  chisq.test()

# Neither method finds a significant association between PhysActiveDays
# and BPSysAve.