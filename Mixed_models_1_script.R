library(tidyverse)

gender_height_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/gender_height_data.csv")

head(gender_height_data)

# Mutate subject and gender into factors

gender_height_data <- gender_height_data %>% 
  mutate(subject = factor(subject),
         gender = factor(gender))

gender_height_data %>% 
  ggplot(aes(x = gender, y = height, colour = gender)) +
  geom_point(position = position_jitter(width = 0.1, seed = 42)) +
  guides(colour = 'none') +
  theme_minimal() +
  labs(x = "Gender",
       y = "Height (cm)",
       title = "Relationship between Gender and Height") +
  scale_x_discrete(labels = c("Female", "Male"))

#Building our linear model
