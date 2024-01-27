
library(NHANES)
DATA(NHANES)
?NHANES
data("NHANES")
DATA(NHANES)
names(NHANES)
NHANES$Diabetes
table(NHANES$Diabetes)

NHANES %>% 
  group_by(Diabetes) %>% 
  summary()


NHANES %>% 
  ggplot(aes(x= Diabetes, y= Depressed)) +
  geom_point()

names(NHANES)

# Assuming df is your data frame containing the Diabetes and Depressed variables
# First, we exclude rows with NA in either variable for the purpose of this visualization
df_clean <- na.omit(NHANES[, c("Diabetes", "Depressed")])

# Mosaic plot
mosaicplot(~ Diabetes + Depressed, data = df_clean, main = "Mosaic Plot of Diabetes vs. Depressed", color = TRUE)

ggplot(df_clean, aes(x = Diabetes, fill = Depressed)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Diabetes by Depressive Symptoms",
       x = "Diabetes",
       y = "Percentage",
       fill = "Depressed") +
  theme_minimal()

summary (NHANES)
