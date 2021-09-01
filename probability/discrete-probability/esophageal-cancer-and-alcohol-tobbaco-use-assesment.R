# Case-control studies help determine whether certain exposures 
# are associated with outcomes such as developing cancer. The 
# built-in dataset esoph contains data from a case-control
# study in France comparing people with esophageal 
# cancer (cases, counted in ncases) to people without esophageal
# cancer (controls, counted in ncontrols) that are carefully 
# matched on a variety of demographic and medical characteristics.
# The study compares alcohol intake in grams per day (alcgp) and
# tobacco intake in grams per day (tobgp) across cases and controls
# grouped by age range (agegp).

library(tidyverse)
head(esoph)

# Each row contains one group of the experiment. Each group has a different 
# combination of age, alcohol consumption, and tobacco consumption. 
# The number of cancer cases and number of 
# controls (individuals without cancer) are reported for each group.

# How many groups are in the study?
nrow(esoph)

# How many cases are there?
all_casses = sum(esoph$ncases)

# How many controls are there?
all_controls = sum(esoph$ncontrols)

# What is the probability that a subject in the highest alcohol consumption 
# group is a cancer case?

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# What is the probability that a subject in the lowest alcohol consumption 
# group is a cancer case?

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Given that a person is a case, what is the probability that
# they smoke 10g or more a day?

more_than_10_g_smokers = c("10-19", "20-29", "30+")
tobacco_cases = esoph %>%
  filter(tobgp %in% more_than_10_g_smokers) %>%
  pull(ncases) %>%
  sum()
tobacco_cases / all_casses


# Given that a person is a control, what is the probability that
# they smoke 10g or more a day?

tobacco_controls = esoph %>%
  filter(tobgp %in% more_than_10_g_smokers) %>%
  pull(ncontrols) %>%
  sum()
tobacco_controls / all_controls

# For cases, what is the probability of being in the highest alcohol group?

highest_alcohol_group = "120+"
high_alcohol_cases = esoph %>% 
  filter(alcgp == highest_alcohol_group) %>%
  summarize(sum(ncases))
probability_high_alcohol_cases = high_alcohol_cases / all_casses

# For cases, what is the probability of being in the highest tobacco group?
highest_tobbaco_group = "30+"
high_tobbaco_cases = esoph %>% 
  filter(tobgp == highest_tobbaco_group) %>%
  summarize(sum(ncases))
high_tobbaco_cases / all_casses

# For cases, what is the probability of being in the highest alcohol group and 
# the highest tobacco group?

high_tobbaco_and_alcohol_cases = esoph %>% 
  filter(tobgp == highest_tobbaco_group & alcgp == highest_alcohol_group) %>%
  summarize(sum(ncases))
high_tobbaco_and_alcohol_cases / all_casses

# For cases, what is the probability of being in the highest alcohol group or 
# the highest tobacco group?

high_tobbaco_or_alcohol_cases = esoph %>% 
  filter(tobgp == highest_tobbaco_group | alcgp == highest_alcohol_group) %>%
  summarize(sum(ncases))
probability_high_tobbaco_or_alcohol_cases = 
  high_tobbaco_or_alcohol_cases / all_casses

# For controls, what is the probability of being in the highest alcohol group?

high_alcohol_controls = esoph %>% 
  filter(alcgp == highest_alcohol_group) %>%
  summarize(sum(ncontrols))
probability_high_alcohol_controls = high_alcohol_controls / all_controls

# How many times more likely are cases than controls to be in the
# highest alcohol group?

probability_high_alcohol_cases / probability_high_alcohol_controls

# For controls, what is the probability of being in the highest tobacco group?

high_tobbaco_controls = esoph %>% 
  filter(tobgp == highest_tobbaco_group) %>%
  summarize(sum(ncontrols))
probability_high_tobbaco_controls = high_tobbaco_controls / all_controls

# For controls, what is the probability of being in the highest alcohol group 
# and the highest tobacco group?

high_tobbaco_and_alcohol_controls = esoph %>% 
  filter(tobgp == highest_tobbaco_group & alcgp == highest_alcohol_group) %>%
  summarize(sum(ncontrols))
high_tobbaco_and_alcohol_controls / all_controls

# For controls, what is the probability of being in the highest alcohol group or 
# the highest tobacco group?

high_tobbaco_or_alcohol_controls = esoph %>% 
  filter(tobgp == highest_tobbaco_group | alcgp == highest_alcohol_group) %>%
  summarize(sum(ncontrols))
probability_high_tobbaco_or_alcohol_controls = 
  high_tobbaco_or_alcohol_controls / all_controls

# How many times more likely are cases than controls to be in the highest 
# alcohol group or the highest tobacco group?

probability_high_tobbaco_or_alcohol_cases / probability_high_tobbaco_or_alcohol_controls
  