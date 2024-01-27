## Only install the libraries below when you run this script for the first time.

# install.packages("NHANES") # data package
library(tidyverse)
library(NHANES)

# Let's load the data in
data("NHANES")

# simplest histogram
NHANES %>% 
  ggplot(aes(x = Pulse)) +
  geom_histogram()
# Notice the warning messages telling you that the number of bins was set to the default 30
# You can play around with the number of bins by putting bins = 10 (or another number of your choice)
# inside the brackets after geom_histogram

# slightly nicer histogram
# In this version, we will change the number of bins to 10 (same as in our frequency table)
# And we give details about the x-axis - we want it to run from 40 to 150,
# with a tick every 10 points
# Compare this histogram with the previous version using the arrows on the Plots panel

NHANES %>% 
  ggplot(aes(x = Pulse)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(breaks=seq(40,150,10))

# Boxplot
# This is the default boxplot, and already looks fine
# Notice how we put the Pulse variable on the y-axis (rather than x-axis, as we did in histogram)
# It's a convention that boxplots are usually vertical rather than horizontal

NHANES %>% 
  ggplot(aes(y = Pulse)) +
  geom_boxplot()

# Multiple distributions at once

# Let's go back to our simple histogram, with 10 bins:
NHANES %>% 
  ggplot(aes(x = Pulse)) +
  geom_histogram(bins = 10)

# Now we would like to see the distribution of pulse depending on the level of physical activity
# We can do this using a function called facet_wrap
# Run the chunk below and see what happens:

NHANES %>% 
  ggplot(aes(x = Pulse)) +
  geom_histogram(bins = 10) +
  facet_wrap(facets = NHANES$PhysActive) 

# You should see 3 separate histograms, each for a different level of the PhysActive variable
# (and a third one for people who had missing data on that variable)

# Let's add a nice title to this graph:
NHANES %>% 
  ggplot(aes(x = Pulse)) +
  geom_histogram(bins = 10) +
  facet_wrap(facets = NHANES$PhysActive) +
  labs(title = "Distribution of pulse depending on \nvigorous physical activity")

# Let's do a similar exploration using a boxplot:
NHANES %>% 
  ggplot(aes(x = PhysActive, y = Pulse)) +
  geom_boxplot() +
  labs(title = "Do physically active people have lower pulse?",
       x = "Moderate or vigorous physical activity")

# Notice that we put Pulse on the y-axis, and level of physical activity on the x axis
# to stick with the convention that boxplots are vertical
# We've added a title and a label to the x-axis

library(dplyr)
library(forcats)
NHANES %>% 
  ggplot(aes(x = PhysActive, y = Pulse)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width = .4) +
  coord_flip() +
  xlab("") +
  theme_bw()


