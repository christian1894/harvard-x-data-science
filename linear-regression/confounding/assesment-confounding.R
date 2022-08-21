# Assessment: Confounding

library(dslabs)
library(tidyverse)
library(ggplot2)
library(broom)
data("research_funding_rates")

# Construct a two-by-two table of gender (men/women) by 
# award status (awarded/not) using the total numbers 
# across all disciplines.

total_awarded_men = sum(research_funding_rates$awards_men)
total_awarded_women = sum(research_funding_rates$awards_women)
total_not_awarded_men = 
  research_funding_rates %>% 
  summarize(sum(applications_men) - sum(awards_men)) %>% 
  pull()
total_not_awarded_women = 
  research_funding_rates %>% 
  summarize(sum(applications_women) - sum(awards_women)) %>% 
  pull()


two_by_two_table = matrix(c(total_awarded_men,
                            total_awarded_women,
                            total_not_awarded_men,
                            total_not_awarded_women),2,2)
colnames(two_by_two_table)<-c("Awarded","Not Awarded")
rownames(two_by_two_table)<-c("Men","Women")

# What is the number of men not awarded?

two_by_two_table[1,2]

# What is the number of women not awarded?

two_by_two_table[2,2]

# What is the percentage of men awarded?

total_applicants_men = research_funding_rates %>%
  summarize(sum(applications_men)) %>%
  pull()
round((total_awarded_men /  total_applicants_men) * 100, 1)

# What is the percentage of women awarded?

total_applicants_women = research_funding_rates %>%
  summarize(sum(applications_women)) %>%
  pull()
round((total_awarded_women /  total_applicants_women) * 100, 1)

# Run a chi-squared test External link on the two-by-two
# table to determine whether the difference in the two
# funding awarded rates is significant. (You can use tidy() 
# to turn the output of chisq.test() into a data
# frame as well.)

chi_sq = chisq.test(two_by_two_table) %>% tidy()

# What is the p-value of the difference in funding awarded rate?

chi_sq$p.value

# There may be an association between gender and funding.
# But can we infer causation here? Is gender bias causing 
# this observed difference? The response to the original
# paper claims that what we see here is similar to the 
# UC Berkeley admissions example. Specifically they state 
# that this "could be a prime example of Simpson’s paradox;
# if a higher percentage of women apply for grants in 
# more competitive scientific disciplines, then an analysis 
# across all disciplines could incorrectly show 'evidence' 
# of gender inequality."

# To settle this dispute, use this dataset with number 
# of applications, awards, and success rate for each gender:



dat = research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")

# To check if this is a case of Simpson's paradox, plot
# the success rates versus disciplines, which have been 
# ordered by overall success, with colors to denote the 
# genders and size to denote the number of applications.

dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()
