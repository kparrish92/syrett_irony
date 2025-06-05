library(brms)
library(here)

approve_data = read.csv(here("data", "tidy", "approve_data_tidy.csv"))


mod_int = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                      (1 | trial_object) + (1 | trial_modifier),
                    family = "bernoulli",
                    data = approve_data %>% filter(modifier_type == "grad+subj+pos"),
                    file = here("data", "models", "approve_mod_grad_s_p.rds"))

mod_int2 = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                      (1 | trial_object) + (1 | trial_modifier),
                    family = "bernoulli",
                    data = approve_data %>% filter(modifier_type == "grad+subj+neg"),
                    file = here("data", "models", "approve_mod_grad_s_n.rds"))

mod_int3 = brms::brm(response ~ intonation*group + (1 | subject.ID) + 
                      (1 | trial_object) + (1 | trial_modifier),
                    family = "bernoulli",
                    data = approve_data %>% filter(modifier_type == "–grad–subj"),
                    file = here("data", "models", "approve_mod_gs.rds"))


conditional_effects(mod_int)
conditional_effects(mod_int2)
conditional_effects(mod_int3)



