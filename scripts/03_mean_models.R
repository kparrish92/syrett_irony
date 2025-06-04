library(brms)

mean_data = read.csv(here("data", "tidy", "mean_data_tidy.csv"))


m_mod_int = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                      (1 | trial_object) + (1 | trial_modifier),
                    family = "bernoulli",
                    data = mean_data %>% filter(modifier_type == "grad+subj+pos"),
                    file = here("data", "models", "mean_mod_grad_s_p.rds"))

m_mod_int2 = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                       (1 | trial_object) + (1 | trial_modifier),
                     family = "bernoulli",
                     data = mean_data %>% filter(modifier_type == "grad+subj+neg"),
                     file = here("data", "models", "mean_mod_grad_s_n.rds"))

m_mod_int3 = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                       (1 | trial_object) + (1 | trial_modifier),
                     family = "bernoulli",
                     data = mean_data %>% filter(modifier_type == "–grad–subj"),
                     file = here("data", "models", "mean_mod_gs.rds"))


conditional_effects(m_mod_int)
conditional_effects(m_mod_int2)
conditional_effects(m_mod_int3)


