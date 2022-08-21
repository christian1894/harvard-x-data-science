# Confounders

library(tidyverse)
library(dslabs)
library(ggplot2)
library(broom)

# Confounders are perhaps the most common reason that 
# leads to associations begin misinterpreted.
# If X and Y are correlated, we call Z a confounder if 
# changes in Z causes changes in both X and Y. Earlier, 
# when studying baseball data, we saw how Home Runs was a 
# confounder that resulted in a higher correlation than 
# expected when studying the relationship between Bases 
# on Balls and Runs. In some cases, we can use linear 
# models to account for confounders. However, this is
# not always the case.

# Incorrect interpretation due to confounders is 
# ubiquitous in the lay press and they are often 
# hard to detect. Here, we present a widely used 
# example related to college admissions.

# Example: UC Berkeley admissions

# Admission data from six U.C. Berkeley majors, 
# from 1973, showed that more men were being 
# admitted than women: 44% men were admitted 
# compared to 30% women. PJ Bickel, EA Hammel, 
# and JW O’Connell. Science (1975). We can load 
# the data and a statistical test, which clearly
# rejects the hypothesis that gender and admission 
# are independent:

data(admissions)
admissions %>%
  group_by(gender) %>%
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
         not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>%
  summarize(tidy(chisq.test(.))) %>% .$p.value

# But closer inspection shows a paradoxical result. Here 
# are the percent admissions by major:

admissions %>% 
  select(major, gender, admitted) %>%
  pivot_wider(names_from = "gender", values_from = "admitted") %>%
  mutate(women_minus_men = women - men)

# Four out of the six majors favor women. More importantly,
# all the differences are much smaller than the 14.2 
# difference that we see when examining the totals.

# The paradox is that analyzing the totals suggests a 
# dependence between admission and gender, but when 
# the data is grouped by major, this dependence seems 
# to disappear. What’s going on? This actually can 
# happen if an uncounted confounder is driving most 
# of the variability.

# So let’s define three variables: X is 1 for men and 0 
# for women, Y is 1 for those admitted and 0 
# otherwise, and Z quantifies the selectivity of the 
# major. A gender bias claim would be based on the 
# fact that Pr(Y=1|X=x) is higher for x=1 than x=0. 
# However, Z is an important confounder to consider.
# Clearly Z is associated with Y, as the more selective 
# a major, the lower Pr(Y=1|Z=z). But is major 
# selectivity Z associated with gender X?
# One way to see this is to plot the total percent 
# admitted to a major versus the percent of women 
# that made up the applicants:

admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants)/sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# There seems to be association. The plot suggests that 
# women were much more likely to apply to the two “hard” 
# majors: gender and major’s selectivity are confounded. 
# Compare, for example, major B and major E. Major E is 
# much harder to enter than major B and over 60% of 
# applicants to major E were women, while less than 
# 30% of the applicants of major B were women.

# Confounding explained graphically
# The following plot shows the number of applicants 
# that were admitted and those that were not by:

admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

# It also breaks down the acceptances by major. This 
# breakdown allows us to see that the majority of 
# accepted men came from two majors: A and B. It also 
# lets us see that few women applied to these majors.

# Average after stratifying

# In this plot, we can see that if we condition or
# stratify by major, and then look at differences, 
# we control for the confounder and this effect goes away:

admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

# Now we see that major by major, there is not much 
# difference. The size of the dot represents the number 
# of applicants, and explains the paradox: we see
# large red dots and small blue dots for the easiest 
# majors, A and B.
# If we average the difference by major, we find that 
# the percent is actually 3.5% higher for women.

admissions %>%  
  group_by(gender) %>% summarize(average = mean(admitted))
