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
  labs(x = "Condition",
       y = "Gaze Duration (ms.)",
       title = "The effect of Item Valence on Gaze Duration") +
  coord_flip())

# If we attempt to build a model which takes into account the random effect of condition, item and subject, we get an
# warning suggesting we have too many parameters than our data supports:

factor_1_model <- lmer(Gaze ~ Condition + (1 + Condition | Subject) + (1 + Condition | Item), data = mixed_model_gaze)

# We can simplify the model by dropping random effects until the warning message disappears...

factor_1_model <- lmer(Gaze ~ Condition + (1 | Subject) + (1 | Item), data = mixed_model_gaze)

# Let's check whether our normality assumptions have been violated. We'll need the performance package for this

library(performance)

check_model(factor_1_model)

# These look pretty good - not perfect, but still useful.

summary(factor_1_model)

# In this case, the reference contrast relates to the negative condition, since it orders it alphabetically.
# We can see that, in comparison to the negative stimuli, neutral and positive stimuli were significantly different.
# Let's run some pairwise comparisons to determine whether each condition differs from each other. By default, we use
# Tukey comparison

library(emmeans)

emmeans(factor_1_model, pairwise ~ Condition)

# Now that we have adjusted for multiple comparisons, we can see that only one of the conditions was positive - negative
# vs positive stimuli. 