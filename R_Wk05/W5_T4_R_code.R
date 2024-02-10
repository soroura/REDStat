# Load the tidyverse package
library(tidyverse)

# Load the NHANES package
library(NHANES)

# Plot a positive regression line
data <- tibble(y=c(8, 9, 10, 9, 11, 14, 15, 13, 14, 15, 17, 16, 19, 18, 20, 21),
               x=c(0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 8, 9, 9, 11, 12, 12))

png("regression_line.png",width = 165, height = 75, units='mm', res = 300)
ggplot(data,aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  xlim(0,12.5) +
  theme_bw() +
  labs(x='X Values', y='Y Values')
dev.off()

# Plot a negative regression line
data <- tibble(y=c(21,20,18,19,16,17,15,14,13,15,14,11,9,10,9,8),
               x=c(0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 8, 9, 9, 11, 12, 12))

png("regression_line_negative.png",width = 165, height = 75, units='mm', res = 300)
ggplot(data,aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  xlim(0,12.5) +
  theme_bw() +
  labs(x='X Values', y='Y Values')
dev.off()

# Fit a linear model to the data
lmodel <- lm(y ~ x, data = data)
lmodel$coefficients
summary(lmodel)
confint(lmodel)