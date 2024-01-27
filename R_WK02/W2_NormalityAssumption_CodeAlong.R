# Demonstration of checking the normality assumption

# Load the tidyverse package
library(tidyverse)

# Install the NHANES package with the dataset (only do this once!)
# install.packages("NHANES")

# Load the NHANES package
library(NHANES)


###############################################################################
# EXAMPLE: Using a subset of NHANES with only male individuals aged 18 and over,
#            check the normality assumption for the Height variables. 
###############################################################################

# Create a subset of the NHANES dataset with only male individuals aged 18 and over,
# who have Height information
subsetNHANES <-NHANES %>%
  filter(Gender == "male", Age >= 18) %>% ## keep only male individuals aged 18 and over
  drop_na(Height)                         ## remove rows where Height is missing (NA)

# Checking the assumption of normal distribution of Height variable

# Plot a histogram for Height
subsetNHANES %>%                     ## Using the subsetNHANES filtered dataset,
  ggplot(aes(x = Height)) +          ## set up the ggplot object with Height on the x-axis,
  geom_histogram(col="black",        ## create the histogram with black lines around the boxes,
                 fill="lightblue",   ## filled with the colour lightblue
                 bins = 30 ) +       ## and with 30 bins in total
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

## The histogram has the symmetric bell shape characteristic of the normal distribution,
## therefore the histogram suggests that the Height variable comes from a normal distribution.

# Create a Normal Q-Q plot for Height
subsetNHANES %>%                     ## Using the subsetNHANES filtered dataset,
  ggplot(aes(sample=Height)) +       ## set up the ggplot object with Height as the sample
  stat_qq() +                        ## draw the Q-Q plot
  stat_qq_line(color=2) +            ## draw the normal line in red
  labs(title="Normal Q-Q Plot") +    ## add title
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

## The Normal Q-Q plot follows the straight line very closely, despite a small deviation
## at both ends of the distribution. The Q-Q plot also suggests a normal distribution.

# Perform the Shapiro-Wilk test on the Height variable
subsetNHANES %>%        ## Using the subsetNHANES filtered dataset,
  pull(Height) %>%      ## take the Height variable and use it as a vector
  shapiro.test()        ## perform the Shapiro-Wilk test on that Height vector

# Base R alternative
shapiro.test(subsetNHANES$Height)

## The Shapiro-Wilk test of normality has a p-value of 0.02, this is below the
## significance level of 0.05, therefore we can reject the null hypothesis that 
## Height comes from a normal distribution, in other words the Height variable 
## does not come from a normal distribution. However, 0.02 is quite close to 0.05,
## and a small deviation will cause a significant result in large samples.
## Here, we have n=3658 observations

# Test for normality of Height using the Kolmogorov-Smirnov test
subsetNHANES %>%                  ## Using the subsetNHANES filtered dataset,
  pull(Height) %>%                ## take the Height variable and use it as a vector
  ks.test(.,                      ## perform the Kolmogorov-Smirnov test on that Height vector
          "pnorm",                ## set the theoretical distribution as the normal probability distribution
          mean=mean(.),sd=sd(.))  ## estimate the mean and standard deviation from the Height sample

# Here you will get warnings about ties as the Kolmogorov-Smirnov test doesn't 
# expect any ties in a continuous distribution.
## The Kolmogorov-Smirnov test has a p-value of 0.27, this is above the significance
## level of 0.05, therefore we cannot reject the null hypothesis that Height comes
## from a normal distribution, in other words, the Height variable comes from a normal distribution.

## Overall, it seems to be reasonable to assume that Height is normally distributed.

