# library
library(ggplot2)

# dataset:
data=data.frame(value=rnorm(100))

# basic histogram
p <- ggplot(data, aes(x=value)) + 
  geom_histogram()
