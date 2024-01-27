library(NHANES)
library(tidyverse)

install.packages("NHANES")
data(NHANES)

# We'll start with a histogram of height from the NHANES dataset
# I picked height because I assume it will be normally distributed among adults

NHANES %>% 
  filter(Age > 17) %>% # filter the data to only include adults (18 or above)
  ggplot(aes(x = Height)) +
  geom_histogram(bins = 20) + # 20 bins was just a guess - you can try a different number
  labs(title = "Height of adults in the NHANES study") # adding a title is always a good idea

# adding the probability density function
# notice the change on line 22 - we're scaling the histogram to have density instead of count
# and we are adding a probability density line on line 23

NHANES %>% 
  filter(Age > 17) %>% 
  ggplot(aes(x = Height)) +
  geom_histogram(aes(y=..density..), bins = 20) +  # scale histogram y-axis
  geom_density(col = "red")

# This is how we added the blue and black line on slide 10
# geom_vline stands for a vertical line
# we have to specify the intercept, colour and size

NHANES %>% 
  filter(Age > 17) %>% 
  ggplot(aes(x = Height)) +
  geom_density(col = "red") +
  geom_vline(aes(xintercept = 180), color="blue", size=1) +
  geom_vline(aes(xintercept = 190), color="black", size=1)
