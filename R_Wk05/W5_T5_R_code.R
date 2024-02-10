library(tidyverse)
library(NHANES)

# Create a subset of the NHANES dataset with only male individuals aged 18 and over,
# who have both Height and Weight information
subsetNHANES <-NHANES %>%
  filter(Gender == "male", Age >= 18) %>%
  drop_na(Height,Weight) %>% # remove rows where either Height or Weight is NA
  select(Height,Weight)

# Create a scatterplot for Height (x axis) and Weight (y axis)
subsetNHANES %>%
  ggplot(aes(x = Height, y = Weight)) + 
  geom_point(pch=20) + # plot the data points and change the dot shape / size
  xlab("Height (m)") + 
  ylab("Weight (kg)") +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Create a subset of the NHANES dataset with 5000 random individuals
# who have both Height and Weight information
subsetNHANES <-NHANES %>%
  sample_n(5000) %>%
  drop_na(Height,Weight) %>% # remove rows where either Height or Weight is NA
  select(Height,Weight)

# Fit a regression model to predict Weight from Height
model <- lm(Weight~Height, data=subsetNHANES)

# Get a list of residuals for that model
res <- resid(model)

# Produce a residual vs. fitted plot
ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),color="red") +
  labs(y='Residuals',x='Fitted values') +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

## Made up data to create residual plots for normal and non-normal residuals
set.seed(127) # This sets a seed of 127 so the random data will remain the same
# Create a vector x for numbers 1 to 200
x<-seq(1,200,1)

# Create an error term dependent on x named err
err<-NULL
for(i in c(1:200)) {
  err[i]<-rnorm(1,0,i)
}
# Calculate y using the equation y = 3.8x + 25 + err
y<-3.8*x+25+err

# Linear regression to predict y from x
naivelm<-lm(y~x)

# Calculate the absolute value of the linear model's residuals
absresid<-abs(naivelm$residuals)

# Constant variability
# Create a normally distributed error term called errno
errno<-NULL
for(i in c(1:200)) {
  errno[i]<-rnorm(1,0,15)
}
# Calculate yn using the equation y = 3.8x + 25 + errno
yn<-3.8*x+25+errno

# Linear regression to predict yn from x
normallm<-lm(yn~x)

## Residual plots for both models, displayed one above the other
par(mfrow=c(2,1))
# Residual plot for naivelm, non-constant variability
ggplot(naivelm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),color="red") +
  labs(title="Heteroskedastic Residuals",y='Residuals',x='Fitted values') +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Residual plot for normallm, constant variability
ggplot(normallm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),color="red") +
  labs(title="Homoskedastic Residuals",y='Residuals',x='Fitted values') +
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid


# Normality of residuals for model for subsetNHANES to predict Weight from Height
# Create a Q-Q plot
model %>%
  ggplot(aes(sample=.resid)) +
  stat_qq() +
  stat_qq_line(color=2) +
  labs(title="Normal Q-Q Plot") +    ## add title
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

# Create a histogram
model %>%
  ggplot(aes(x=.resid)) +
  geom_histogram(col="black",fill="lightblue",bins = 30) +
  labs(title="Histogram of residuals") +    ## add title
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

