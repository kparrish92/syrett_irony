

### Load adult and child data 

adult_data = read.csv(here("data", "tidy_data.csv")) %>% 
  filter(trial_number > 6) %>% 
  select(response, intonation, modifier_type, subject.ID, trial_object, probe_type, trial_modifier) %>% 
  mutate(group = "adults")

child_data = read.csv(here("data", "child_data.csv")) %>% 
  filter(trial_number > 6) %>% 
  filter(response != "N/A") %>% 
  select(response, intonation, modifier_type, subject.ID, trial_object, probe_type, trial_modifier) %>% 
  mutate(group = "children")

### Get the ordinal data and bind it 

adult_data_ordinal = adult_data %>% 
  filter(probe_type == "nice") 

child_data_ordinal = child_data %>% 
  filter(probe_type == "nice")

ordinal_data_tidy = rbind(adult_data_ordinal, child_data_ordinal)

ordinal_data_tidy %>% 
  write.csv(here("data", "tidy", "ordinal_data_tidy.csv"))
  
### Recode the adult data in approve and mean types 

#### approve 
adult_data_approve = adult_data %>% 
  filter(probe_type == "approve") %>% 
  mutate(response = case_when(
    response == 1 ~ 0,
    response == 2 ~ 1
  ))

child_data_approve = child_data %>% 
  filter(probe_type == "approve")


approve_data_tidy = rbind(adult_data_approve, child_data_approve)

approve_data_tidy %>% 
  write.csv(here("data", "tidy", "approve_data_tidy.csv"))

#### mean 

adult_data_mean = adult_data %>% 
  filter(probe_type == "mean") %>% 
  mutate(response = case_when(
    response == 1 ~ 0,
    response == 2 ~ 1
  ))

child_data_mean = child_data %>% 
  filter(probe_type == "mean")


mean_data_tidy = rbind(adult_data_mean, child_data_mean)

mean_data_tidy %>% 
  write.csv(here("data", "tidy", "mean_data_tidy.csv"))