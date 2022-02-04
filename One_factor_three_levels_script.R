mixed_model_gaze <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/one_factor.csv")

head(mixed_model_gaze)

mixed_model_gaze <- mixed_model_gaze %>% 
  mutate(Condition = factor(Condition),
         Subject = factor(Subject),
         Item = factor(Item))

