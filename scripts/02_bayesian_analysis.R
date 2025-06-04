library(tidyverse)
library(here)
library(bayestestR)
library(bayesplot)
library(brms)
library(report)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

syrett_theme <- function() {
  theme(
    axis.text = element_text(colour = "black", family = "Arial", size = 12),
    axis.title = element_text(colour = "black", family = "Arial", size = 12))
} 

tidy_data = read.csv(here("data", "tidy_data.csv")) %>% 
  filter(trial_number > 6) # removing practice trials 

child_data = read.csv(here("data", "child_data.csv")) %>% 
  filter(trial_number > 6) %>% 
  filter(response != "N/A")

adult_data = read.csv(here("data", "tidy_data.csv")) %>% 
  filter(trial_number > 6) %>% # removing practice trials %>% 
  select(response, intonation, modifier_type, subject.ID, trial_object, probe_type, trial_modifier) %>% 
  mutate(group = "adults")

child_data_n = read.csv(here("data", "child_data.csv")) %>% 
  filter(trial_number > 6) %>% 
  filter(response != "N/A") %>% 
  select(response, intonation, modifier_type, subject.ID, trial_object, probe_type, trial_modifier) %>% 
  mutate(group = "children")

full_df = rbind(adult_data, child_data_n) 

full_df$response = as.integer(full_df$response)

full_df_nice = full_df %>% filter(probe_type == "nice") %>% 
  filter(!is.na(response))

full_df_mean = full_df %>% filter(probe_type == "mean") %>% 
  mutate(response = case_when(
    response == 1 ~ 0,
    response == 2 ~ 1
  )) %>% 
  filter(!is.na(response))

full_df_approve = full_df %>% filter(probe_type == "approve") %>% 
  mutate(response = case_when(
    response == 1 ~ 0,
    response == 2 ~ 1
  )) %>% 
  filter(!is.na(response))





### Nice models

approve_mod1 = full_df_approve %>% 
#  filter(modifier_type == "grad+subj+pos") %>% 
  filter(subject.ID == "AP_04")


unique(approve_mod1$response)
unique(approve_mod1$modifier_type)
unique(approve_mod1$intonation)
unique(approve_mod1$subject.ID)


ef = approve_mod1 %>% 
  group_by(subject.ID) %>% 
  summarise(n = n())


mod_int = brms::brm(response ~ intonation + (1 | subject.ID) + 
                              (1 | trial_object) + (1 | trial_modifier),
                            family = "bernoulli",
                            data = approve_mod1,
                            file = here("data", "models", "mod1_int_approve.rds"))


mod_grp_int = brms::brm(response ~ intonation*group + (1 | subject.ID),
                    family = "bernoulli",
                    control = list(max_treedepth = 15),
                    data = approve_mod1,
                    file = here("data", "models", "mod1_int_grp_approve.rds"))



check = approve_mod1 %>% 
  group_by(intonation,group, subject.ID, response) %>% 
  summarise(n = n())


### Predict "yes", "no" or "maybe" as a function of intonation, group and their interaction. 


check = approve_mod1 %>% 
  group_by(intonation, group, subject.ID, response) %>% 
  summarise(n = n())

check = approve_mod1 %>% 
  group_by(subject.ID) %>% 
  summarise(n = n())




describe_posterior(
  mod_grp_int,
  effects = "fixed",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all") %>%
  as.data.frame() %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  knitr::kable(row.names = FALSE)

conditional_effects(mod_grp_int)


