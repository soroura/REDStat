# Load the tidyverse package
library(tidyverse)

# Load the NHANES package
library(NHANES)

# Take the NHANES dataset, then filter to keep only adult males, then create a scatter plot of Weight against Height
NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  ggplot(aes(x = Height, y = Weight)) + 
  geom_point() + 
  xlab("Height (cm)") + 
  ylab("Weight (kg)")

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

# Fit a linear regression model to the data
lmodel <- lm(y ~ x, data = data)

# Plot the positive regression line with residuals
intercept <- lmodel$coefficients[1]
slope <- lmodel$coefficients[2]
data$fitted <- intercept + slope * data$x

png("regression_line_residuals.png",width = 165, height = 75, units='mm', res = 300)
ggplot(data, aes(x = x, y = y)) +
  geom_abline(slope = slope, intercept = intercept, color = "turquoise4",size=1) +
  geom_segment(aes(xend = x, yend = fitted, color = "resid",size=1)) +
  scale_size_identity() +
  geom_point() +
  theme_bw() +
  theme(legend.position="none") +
  labs(x='X Values', y='Y Values') +
  scale_color_manual(values = c(resid = "darkred"), labels = c(resid = "residuals"))
dev.off()