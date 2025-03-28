library(tidyverse)
library(here)
library(brms)
library(lme4)

tidy_data = read.csv(here("data", "tidy_data.csv")) %>% 
  filter(trial_number > 6) # removing practice trials 

syrett_theme <- function() {
  theme(
    axis.text = element_text(colour = "black", family = "Arial", size = 12),
    axis.title = element_text(colour = "black", family = "Arial", size = 12))
} 


length(unique(tidy_data$subject.ID))# check no. ppts 

length(unique(tidy_data$modifier_type)) # check unique modifier types

length(unique(tidy_data$probe_type)) # check probe types 

mean_df = tidy_data %>% 
  filter(probe_type == "mean") 


mean_df %>% 
  ggplot(aes(x = intonation, fill = modifier_type)) + 
  geom_bar(position = "dodge", color = "black") + facet_wrap(~response) +
  theme_minimal() + syrett_theme()

mean_mod = brms::brm(response ~ intonation*modifier_type + (1 | subject.ID) + 
                            (1 | trial_object) + (1 | trial_modifier),
                          family = "bernoulli",
                          data = mean_df)

conditional_effects(mean_mod)

approve_df = tidy_data %>% 
  filter(probe_type == "approve") 

approve_df %>% 
  ggplot(aes(x = intonation, fill = modifier_type)) + 
  geom_bar(position = "dodge", color = "black") + facet_wrap(~response) +
  theme_minimal() + syrett_theme()

approve_mod = brms::brm(response ~ intonation*modifier_type + (1 | subject.ID) + 
                       (1 | trial_object) + (1 | trial_modifier),
                     family = "bernoulli",
                     data = approve_df)

conditional_effects(approve_mod)


nice_df = tidy_data %>% 
  filter(probe_type == "nice") 

nice_df %>% 
  ggplot(aes(x = response, fill = modifier_type)) + 
  geom_bar(position = "dodge", color = "black") + facet_wrap(~modifier_type) +
  theme_minimal() + syrett_theme()

nice_mod = brms::brm(response ~ intonation*modifier_type + (1 | subject.ID) + 
                          (1 | trial_object) + (1 | trial_modifier),
                        family = "categorical",
                        data = nice_df)

conditional_effects(nice_mod, categorical = TRUE)

