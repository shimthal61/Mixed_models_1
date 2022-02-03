library(tidyverse)

gender_height_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/gender_height_data.csv")

head(gender_height_data)

# Mutate subject and gender into factors

gender_height_data <- gender_height_data %>% 
  mutate(subject = factor(subject),
         gender = factor(gender))

(gender_height_plot <- gender_height_data %>% 
  ggplot(aes(x = gender, y = height, colour = gender)) +
  geom_point(size = 2, position = position_jitter(width = 0.1, seed = 42)) +
  guides(colour = 'none') +
  theme_minimal() +
  labs(x = "Gender",
       y = "Height (cm)",
       title = "Relationship between Gender and Height") +
  scale_x_discrete(labels = c("Female", "Male")))

#Building our linear model
height_model <- lm(height ~ gender, data = gender_height_data)

summary(height_model)

#We can see from our output that the mean height for females (our intercept) is 165cm. On average, men are 12.5cm
# taller (177.5cm). We can also see that our p-value is significant (0.017)

# Let's have a look when we have two continuous variables

age_height_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/age_height_data.csv")

age_height_data %>% 
  mutate(subject = factor(subject))

(age_height_plot <- age_height_data %>% 
    ggplot(aes(x = age, y = height)) +
    geom_point(size = 2) + 
    guides(colour = 'none') +
    theme_minimal() +
    labs(x = "Age (years)", 
         y = "Height (cm)", 
         title = "Relationship between Age and Height"))
    
