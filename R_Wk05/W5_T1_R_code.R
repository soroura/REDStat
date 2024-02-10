# Load the tidyverse package
library(tidyverse)

# Install the NHANES package with the dataset (only do this once!)
# install.packages("NHANES")

# Load the NHANES package
library(NHANES)

# Take the NHANES dataset, then filter to keep only adult males, then create a scatter plot of Weight against Height
NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  ggplot(aes(x = Height, y = Weight)) + 
  geom_point() + 
  xlab("Height (cm)") + 
  ylab("Weight (kg)")

# Using the same dataset as for the scatterplot, we use the summarise function to calculate the Pearson's correlation coefficient
NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  summarise(correlation = cor(Weight, Height, use = "complete.obs"))

# Get the results for the statistical test for the correlation coefficient
Weight <- NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  pull(Weight)

Height <- NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  pull(Height)

cor.test(Weight,Height)