
# logistic model looking for rating of 5 vs below 5 ~ group 

### logistic model shows main effect for version when we predict picking 5 -
### about a difference in probablity of .1 

ef = read.csv(here("data", "ratings_tidy.csv"))

most_int = td %>% filter(question_type == "most_interested_qs")  %>% 
  mutate(rating_recode = case_when(
    rating == 5 | rating == 4 ~ 1,
    rating == 3 | rating == 2 | rating == 1 ~ 0,
  ))

most_int$version = as.factor(most_int$version)
most_int$current_class = as.factor(most_int$current_class)

dir_aff = ef %>% filter(question_type == "directly_affected_questions")
semi_eff = ef %>% filter(question_type == "semi_affected_questions")
least_aff = ef %>% filter(question_type == "least_affected_questions")


null_mod_most = lme4::glmer(rating_recode ~ 1 + (1 | prompt) + (1 | participant_number),
                       family = "binomial",
                       data = most_int)

ver_mod_most = lme4::glmer(rating_recode ~ version + (1 | prompt) + (1 | participant_number),
                      family = "binomial",
                      data = most_int)

class_mod_most = lme4::glmer(rating_recode ~ version + current_class + (1 | prompt) + (1 | participant_number),
                       family = "binomial",
                       data = most_int)

int_mod_most = lme4::glmer(rating_recode ~ version + current_class + current_class:version + (1 | prompt) + (1 | participant_number),
                      family = "binomial",
                      data = most_int)

anova(null_mod_most, ver_mod_most, class_mod_most, int_mod_most)

summary(int_mod_most)
