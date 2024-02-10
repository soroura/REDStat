# Load the tidyverse package
library(tidyverse)

# Set the sample dataset of 8 pairs of x and y values.
x<-c(3,17,6,19,2,13,16,10)
y<-c(4,17,7,23,19,12,21,15)
sample<-tibble(x=x,y=y)

# View the dataset
sample
View(sample)

# Create a scatterplot of the dataset
sample %>% ggplot(aes(x = x, y = y)) + 
  geom_point()

## Steps to calculate  Pearson's correlation coefficient
# Mean and standard deviation of x and y
sumsPearson <- sample %>% summarise(meanx = mean(x), meany = mean(y),stdevx = sd(x), stdevy = sd(y))

# Differences between value and mean for each x and each y
calcPearson <- sample %>%
  summarise(
    diffx = x-sumsPearson$meanx,
    diffy = y-sumsPearson$meany)

# Divide these vectors by the standard deviation
calcPearson <- calcPearson %>%
  mutate(
    diff_sd_x = diffx/sumsPearson$stdevx,
    diff_sd_y = diffy/sumsPearson$stdevy)

# Calculate the product of each value for the above vectors
calcPearson <- calcPearson %>%
  mutate(
    products = diff_sd_x*diff_sd_y)
    
# Sum these products
sumsPearson <- sumsPearson %>% mutate(sumprods = sum(calcPearson$products))

# Divide by (n-1) to get Pearson's coefficient, here is we will use the length of the x vector as n
sumsPearson <- sumsPearson %>% mutate(Pearson_r = sumprods/(length(x)-1))

# View the tibbles we created
calcPearson
sumsPearson

View(calcPearson)
View(sumsPearson)

## Steps to calculate  Spearman's correlation coefficient
# Calculate the rank of each value of x and y
calcSpearman <- sample %>% mutate(rankx = min_rank(x), ranky = min_rank(y))

# Calculate the difference between ranks for each pair of values
calcSpearman <- calcSpearman %>% mutate(diffrank = rankx - ranky)

# Square that difference
calcSpearman <- calcSpearman %>% mutate(squarediffrank = diffrank^2)

# Sum all squared differences
sumsSpearman <- calcSpearman %>% summarise(sumsquarediff = sum(squarediffrank))

# Multiply by 6/n(n2-1)
sumsSpearman <- sumsSpearman %>% mutate(sumsquarediffmultiplied = sumsquarediff*6/(length(x)*(length(x)^2-1)))

# Subtract this number from 1 to get Spearman's coefficient
sumsSpearman <- sumsSpearman %>% mutate(Spearman_rho = 1-sumsquarediffmultiplied)

# View the tibbles we created
calcSpearman
sumsSpearman

View(calcSpearman)
View(sumsSpearman)

# Hypothesis testing: t-test on Pearson's correlation coefficient
# t <- sumsPearson$Pearson_r*sqrt((length(x)-2)/(1-(sumsPearson$Pearson_r)^2))

# SHORTCUT: We can make use of R's cor.test() function to avoid all the steps above!
# Calculate Pearson's correlation coefficient and p-value (default method)
cor.test(x,y)
# This function gives us all at once:
## the method and data used for the test,
## the test statistic, the number of degrees of freedom, the p-value,
## and then the confidence interval and the correlation coefficient.

# Calculate Spearman's correlation coefficient
cor.test(x,y,method="spearman")
