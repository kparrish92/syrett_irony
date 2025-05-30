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
  mutate(response = case_when(
    response == 1 ~ 0,
    response == 2 ~ 1
  )) %>% 
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


combined_full_model_nice = brms::brm(response ~ group*intonation*modifier_type + (1 | subject.ID) + 
                                               (1 | trial_object) + (1 | trial_modifier),
                                             family = "cumulative",
                                             iter = 4000,
                                             data = full_df_nice, 
                                             file = here("data", "models", "full_model_nice.rds"))

combined_full_model_mean = brms::brm(response ~ group*intonation*modifier_type + (1 | subject.ID) + 
                                       (1 | trial_object) + (1 | trial_modifier),
                                     family = "bernoulli",
                                     iter = 4000,
                                     data = full_df_mean, 
                                     file = here("data", "models", "full_model_mean.rds"))

combined_full_model_approve = brms::brm(response ~ group*intonation*modifier_type + (1 | subject.ID) + 
                                       (1 | trial_object) + (1 | trial_modifier),
                                     family = "bernoulli",
                                     iter = 4000,
                                     data = full_df_approve, 
                                     file = here("data", "models", "full_model_approve.rds"))
