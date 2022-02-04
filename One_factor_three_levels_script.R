mixed_model_gaze <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/one_factor.csv")

head(mixed_model_gaze)

mixed_model_gaze <- mixed_model_gaze %>% 
  mutate(Condition = factor(Condition),
         Subject = factor(Subject),
         Item = factor(Item))

(mixed_model_gaze_plot <- mixed_model_gaze %>% 
  ggplot(aes(x = Condition, y = Gaze, colour = Condition)) +
  geom_violin(width = 0.5) +
  geom_point(alpha = 0.2, position = position_jitter(width = 0.1, seed = 42)) +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
  guides(colour = 'none') +
  theme_minimal() +
  labs(x = "Gaze Duration (ms.)",
       y = "Condition",
       title = "The effect of Item Valence on Gaze Duration") +
  coord_flip())
