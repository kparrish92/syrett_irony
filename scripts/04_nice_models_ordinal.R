library(brms)

nice_data = read.csv(here("data", "tidy", "ordinal_data_tidy.csv"))


n_mod_int = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                        (1 | trial_object) + (1 | trial_modifier),
                      family = "cumulative",
                      data = nice_data %>% filter(modifier_type == "grad+subj+pos"),
                      file = here("data", "models", "nice_mod_grad_s_p.rds"))

n_mod_int2 = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                         (1 | trial_object) + (1 | trial_modifier),
                       family = "cumulative",
                       data = nice_data %>% filter(modifier_type == "grad+subj+neg"),
                       file = here("data", "models", "nice_mod_grad_s_n.rds"))

n_mod_int3 = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                         (1 | trial_object) + (1 | trial_modifier),
                       family = "cumulative",
                       data = nice_data %>% filter(modifier_type == "–grad–subj"),
                       file = here("data", "models", "nice_mod_gs.rds"))


conditional_effects(n_mod_int, categorical = TRUE)
conditional_effects(n_mod_int2, categorical = TRUE)
conditional_effects(n_mod_int3, categorical = TRUE)


