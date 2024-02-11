
# About -------------------------------------------------------------------

#author: hossam
#date: 03122023


# pakages -----------------------------------------------------------------


install.packages("outbreaks")
library(outbreaks)

outbreaks::ebola_sierraleone_2014
demo<-ebola_sierraleone_2014
cases<-ebola_sierraleone_2014

str(cases)
summary(cases)
class(cases)
class(cases$date_of_onset)

mean(cases$age, na.rm = TRUE)

median(cases$age, na.rm = TRUE)

range(cases$age, na.rm = TRUE)

summary(cases$age)

summary(cases$date_of_onset)

summary(cases$date_of_sample)

summary(cases$age, na.rm = T)
