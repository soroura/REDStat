# If you haven't used the package janitor before, you'll need to install it now
# It has a lot of helper functions for cleaning and describing data
# install.packages("janitor")

library(NHANES)
library(tidyverse)
library(janitor)

data(NHANES)

# We will be looking at the relationship between height and weight among adults
# We intuitively know that, in this population, the taller people are, the heavier they tend to be
# So, we expect these two variables to be correlated

# Slide 6: simple scatterplot
# Notice that we filter first, to only look at adults
# And then we put height on the x-axis and weight on the y-axis
# geom_point is what we use for a scatterplot
NHANES %>% 
  filter(Age > 17) %>% 
  ggplot(aes(x = Height, y = Weight)) +
  geom_point()

# Slide 8: Now we add a line that illustrates the correlation
# geom_smooth plots the line to help you see a pattern
# I picked a linear model (lm) method for fitting the line
# See what will happen when you change the se argument to TRUE?
NHANES %>% 
  filter(Age > 17) %>% 
  ggplot(aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# Slide 19: Contingency table
# Let's look at a relationship between 2 categorical variables

# Gender and level of physical activity
# tabyl() is a function from the janitor package that creates nice tables
# To make it even nicer, I used the kable() function from the knitr package
NHANES %>% 
  tabyl(Gender, PhysActive) %>% 
  knitr::kable()

# Let's create a similar table for the level of education and race
NHANES %>% 
  tabyl(Education, Race1) %>% 
  knitr::kable()

# Slide 21: visualising two categorical variables
# stacked and grouped bar charts
# We'll focus on gender and the level of ohysical activity

# To make things easier, we'll start by making a table,
# grouping the data into combinations of gender and physical activity
# and counting up the number of observations in each cell (i.e. contingency table, just done another way)

data <- NHANES
data_for_stacked_bar <- NHANES %>% 
  group_by(Gender, PhysActive) %>% 
  summarise(n = n())

# Let's see this table:
data_for_stacked_bar

# Now let's plot a stacked bar chart
# Notice how we have three variables in the aesthetic: gender on the x-axis, number of observations on the y-axis,
# and level of physical activity as the colour filling the bar
# position="stack" determines that the bars will be stacked on top of one another
data_for_stacked_bar %>% 
  ggplot(aes(fill=PhysActive, y=n, x=Gender)) + 
  geom_bar(position="stack", stat="identity")

# Here we plot a grouped bar chart
# Notice that the only difference is in line 77
# where instead of position = "stack", we say position = "dodge",
# telling the bars to be positioned next to one another
data_for_stacked_bar %>% 
  ggplot(aes(fill=PhysActive, y=n, x=Gender)) + 
  geom_bar(position="dodge", stat="identity")
