# Load up the packages we need
library(tidyverse)
library(ggplot2)

# Now were going to look at how the sample mean of our dice rolls is distributed
#
# A sample consists of a set of 100 die rolls
# We will calculate the average value of the die rolls in a sample
#
# We will repeat this process for lots of samples. Then we will have a set 
# of sample means, and we can plot their distribution.

# We are going to do this using a function. A function takes some arguments,
# carries out some procedure, then returns an output.
#
# It is vitally important to know how to write functions in any sort of 
# programming.

# In our case, we will write a function that takes as arguments:
# n = sample size
# And returns the sample mean of that many die rolls

# Set up a vector of probabilities again
probabilities <- rep(1/6, times = 6)

# sample_mean is the name of our function
sample_mean <- function(n){

  # Note how size is set equal to n, which is an input to the function
  rolls <- sample(1:6, size = n, replace = TRUE, prob = probabilities)
  
  return( mean(rolls) )
}


# Let's test out our new function!
# Try running this multiple times, and experiment with different values for n
sample_mean(100)

