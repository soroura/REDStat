## Only install the libraries below when you run this script for the first time.

# install.packages("descriptr") # nicer frequency table
# install.packages("NHANES") # data package
install.packages("descriptr")
library(tidyverse)
library(descriptr)
library(NHANES)

## Slide 6: bar chart for categorical variables

# First, we create a vector of outcomes from tossing a coin
coin_tosses <- c("heads", "heads", "heads", "heads", "heads", "heads",
                 "tails", "tails", "tails", "tails")

# Now we turn this vector into a dataframe for better plotting
# The resulting dataframe has one column
coin_tosses <- as.data.frame(coin_tosses)

# And here we give the dataframe column a name (result)
colnames(coin_tosses) <- c("result")

# Now we create a bar chart
coin_tosses %>% 
  ggplot(aes(x = result)) +
  geom_bar()

## Slide 9: frequency table for a continuous variable 
# we'll use pulse data from the NHANES dataset (and package)
# (10k observations of pulse)

# Let's load the data in first
data(NHANES)

# quite a clunky frequency table, using base R functions
# (gives you the frequency of each value)
table(NHANES$Pulse)

# Using the code below, you can ask R to divide data into bins
# and provide frequencies by bin
bins <- seq(40, 140, by=5)
scores <- cut(NHANES$Pulse, bins)
table(scores) 

# The resulting table is closer to what we'd like, but not great for printing.

# The descriptr package gives us a function for a slightly better frequency table:
ds_freq_table(NHANES, Pulse, 10) # This is the table you saw on slide 9.
            