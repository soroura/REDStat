### Week 1 lab - Describing data

# In this self-guided lab, you will practice exploring data in R using visualisations,
# tables and descriptive statistics.

# The data we will be looking at come from a randomised controlled trial testing whether
# the use of a licorice gargle before intubation for elective thoracic surgery 
# reduces sore throat after the surgery. You can find more information about the dataset
# and a codebook under licorice_gargle here: https://cran.r-project.org/web/packages/medicaldata/medicaldata.pdf

# Let's start by installing the medicaldata package and loading it into R
install.packages("medicaldata")
library(medicaldata)

# We'll be using functions from the tidyverse library, so let's load it in too.
library(tidyverse)

# Now let's tell R that we'll be using the licorice_gargle data
data("licorice_gargle")

# And let's have a quick look at the dataset
str(licorice_gargle)
head(licorice_gargle)

# You'll notice that all variables are recorded as numeric, even though some of them are
# categorical and should be recorded as factors. Let's fix this now, so we have a dataset
# ready for analysis.

licorice_gargle_clean <- licorice_gargle %>% 
  mutate(preOp_gender = factor(preOp_gender, levels = c(0, 1), 
                               labels = c("Male", "Female")),
         preOp_smoking = factor(preOp_smoking, levels = 1:3, 
                                labels = c("Current", "Past", "Never")),
         treat = factor(treat, levels = c(0, 1),
                        labels = c("sugar", "licorice")),
         intraOp_surgerySize = factor(intraOp_surgerySize, levels = 1:3,
                                      labels = c("Small", "Medium", "Large")),
         pacu30min_cough = factor(pacu30min_cough, levels = 0:3,
                                  labels = c("No cough", "Mild", "Moderate", "Severe")))

# Please note: Pain was measured using an 11-point Likert scale, which simply means that
# patients were asked to rate their pain on a scale between 0 and 10. 
# We will analyse data from Likert scales as numeric.

## Exercise 1
# In describing an RCT, we often want to know the demographic and baseline characteristics
# of people in each group. This is to check whether the groups are similar (which is what
# we would expect following a random assignment procedure)
# Please check the baseline characteristics (gender, age, smoking status) of people 
# in both groups. Do they look similar?
# HINT: For categorical variables, try using the table function.
# For numerical variables, first check the distribution (is it normal?), and then
# try the group_by %>% summarise pattern,
# calculating the appropriate statistics to describe the centre and spread.

## Your solution below:
names(licorice_gargle_clean)

licorice_gargle_clean %>% 
   tabyl(preOp_gender, preOp_smoking) %>% 
   knitr::kable()

table(licorice_gargle_clean$treat, licorice_gargle_clean$preOp_gender)

table(licorice_gargle_clean$treat, licorice_gargle_clean$preOp_smoking)


licorice_gargle_clean %>%
  ggplot(aes(x = preOp_age)) +
  geom_histogram() + 
  facet_wrap(~treat)

licorice_gargle_clean %>%
  group_by(treat) %>%
  summarise(median_age = median(preOp_age, na.rm = TRUE),
            IQR_age = IQR(preOp_age, na.rm = TRUE))


## Exercise 2
# Produce a bar chart that shows the distribution of smoking status in each group.
# HINT: Use a grouped bar chart.

## Your solution below:

barchart1 <- licorice_gargle_clean %>%
  ggplot() +
  geom_bar(aes(x= preOp_smoking, fill = treat), position = "dodge")

barchart1  

barchart1 +
  labs(title = "Smoking status in the sugar and licorice group",
       x="Smoking status pre-op",
       y="Number of people", 
       caption = "Based on data from medicaldata package"
       )

## Exercise 3
# Does it look like the licorice gargle reduced throat pain 30 minutes after the surgery?
# And what about cough?
# Use a graph to answer each question. If helpful, you can also create a table.

## Your solution below:
licorice_gargle_clean %>% 
  ggplot(aes(x=treat, y= pacu30min_throatPain)) +
  geom_boxplot()

licorice_gargle_clean %>%
  group_by(treat) %>%
  summarise(mean_pain = mean(pacu30min_throatPain, na.rm = TRUE),
            sd_pain = sd(pacu30min_throatPain, na.rm = TRUE),
            median_pain = median(pacu30min_throatPain, na.rm = TRUE),
            iqr_pain = IQR(pacu30min_throatPain, na.rm = TRUE))

licorice_gargle_clean %>%
  ggplot(aes(x = treat, fill = pacu30min_cough)) +
  geom_bar(position = "dodge")

## Exercise 4
# Is there any evidence that in the intervention (licorice gargle) group, older people
# experienced more post-operative throat pain?
# Create a scatterplot to illustrate the relationship between age and throat pain at 30 minutes
# after the surgery, and comment on your findings. 
# Remember to filter the data, so you only look at the licorice condition. 

## Your solution below:
licorice_gargle_clean %>%
  filter(treat == "licorice") %>%
  ggplot() +
  geom_point(aes(x = preOp_age, y = pacu30min_throatPain)) +
  labs(title = "Do older people experience more pain at 30 minutes post-op?",
       subtitle = "Results from the licorice gargle condition",
       x = "Patient's age in years",
       y = "Throat pain at 30 minutes post-op")
## This is the end of the lab!
## Now you're ready to complete the Week 1: 3 stars and a wish on the Discussion Board.
