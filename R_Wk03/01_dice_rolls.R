# Any line that starts with the character '#' is a 'comment', and is ignored 
# when you run the code. You can use this fact to annotate your code, as I am
# doing right now.

# R files conventionally start with a bunch of library(...) statements
# This loads up packages that other people have written for certain tasks
# For example, tidyverse provides a set of commands for manipulating data
# that is used ubiquitously in R.
# ggplot is a package that is used almost universally for making plots
# in R
# We will be using both of these
library(tidyverse)
library(ggplot2)

# First we are going to set up a vector of probabilities.
# We're going to simulate rolling a die. The probabilities are all 1/6
probabilities <- rep(1/6, times = 6)


# The sample function draws random numbers
#
# The first argument tells it where to draw the numbers from; in this case
# 1,2,3,4,5,6
# The second argument tells it how many draws we want. In this case, 100.
# The third argument tells it whether we want to draw with replacement or not.
# Since we are thinking about rolling a die, we set this to TRUE
# The last argument specifies the probability of each outcome.
rolls <- sample(1:6, size = 100, replace = TRUE, prob = probabilities)


# Right now, rolls is just a list of 100 dice rolls. We want to turn it into a 
# structure called a dataframe.
rolls_df <- data.frame(number_rolled = rolls)

# After running this command, an object will have appeared in the panel on the right
# called rolls_df. Click on it. You will see that it has an index running down 
# the left hand side, and the results of the rolls in a column called 'rolls'


# Now we are going to plot a histogram of the outcome of 100 rolls
rolls_df %>% ggplot(aes(x=number_rolled) ) + 
  geom_histogram(binwidth=1, fill = 'steelblue') +
  scale_x_continuous(breaks = 1:6)

# Have a look at the histogram that just appeared in the Plots tab in the 
# bottom right corner. Does it look like you would expect it to?
