# Load up the packages we need
library(tidyverse)
library(ggplot2)

# Let's continue with what we had before
probabilities <- rep(1/6, times = 6)

sample_mean <- function(n){
  
  # Note how size is set equal to n, which is an input to the function
  rolls <- sample(1:6, size = n, replace = TRUE, prob = probabilities)
  
  return( mean(rolls) )
}

# Now we are going to generate many samples and calculate their mean using
# our function, and a for loop.

# Set the size of our samples
n <- 10

# Create an empty vector. This will be used to store the means of all our samples
means <- c()

# This for loop will calculate the means of 500 samples, where each sample
# consists of 100 die rolls.
for (i in 1:500){
  means <- c(means, sample_mean(n))
}

# Make a dataframe out of our set of sample means
means_df <- data.frame(sample_mean = means)

# Now let's plot a histogram of sample means
means_df %>% ggplot(aes(x=sample_mean) ) + 
  geom_histogram(binwidth = 0.1, aes(y =..count../sum(..count..)),
                 fill = 'steelblue') +
  scale_x_continuous(breaks = seq( from = 1, to = 6, by = 0.5),
                     limits = c(1,6)) +
  # You don't need to fully understand the next few lines of code, but it is just 
  # adding a red line that shows the normal distribution that approximates the 
  # distribution of sample means according to the central limit theorem
  stat_function(fun = function(x) dnorm(x, mean = 3.5, sd = sqrt(105/(36*n) )) * 0.1,
                color = "red", size = 1) + 
  ylab('Density') + xlab('Sample mean')


# TASK: Run this code several times, from line 20 to line 45. Play with different
# values for the sample size n. We started off with 100. Try very small values,
# and very large values. Do you notice anything happening as the sample size gets
# bigger or smaller?
                     