# Association Tests
# We havent't studied statistical tests for ordinal, 
# binary or categorical data; let's look at a case study:
# A 2014 PNAS paper analyzed funding rates in the Netherlands
# and concluded that a gender bias towards male applicants was
# present:
options(digits = 3)
library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates %>% select(discipline, applications_total, 
                                  success_rates_total)
totals = research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals %>% summarize(percent_men = yes_men / (yes_men + no_men),
                     percent_women = yes_women / (yes_women + no_women))
# The main evidence for this conclusion comes down to a comparison
# of percentages, ~18% for men and ~15% for women, we will perform
# inference to see if this difference can be explained by 
# random variability

# Lady Testing Tea
# An acquaintance of R.A Fischer claimed that she could tell
# whether milk was added before or after tea was poured, 
# to test this claim he gave her four pairs of cups of 
# tea: one with milk poured first, the other after. The 
# order was randomized. The null hypothesis here is that 
# she is guessing. Fisher derived the distribution for the 
# number of correct picks on the assumption that the choices
# were random and independent
# Let's say she gets 3 out of 4 correctly, do we assume she
# has a special ability?, for this, we will ask the question
# "If the tester is guessing, what are the chances that she
# gets 3 or more correct?", under this null hyphotesis we can
# think of this experiment as picking 4 blue balls out of an 
# urn with 4 blue and 4 red balls(she knows there are 4 cups
# poured before and 4 after).
# Under the null hypothesis that she is simply guessing, 
# each ball has the same chance of being picked. We can then 
# use combinations to figure out each probability.
# P(picking 3) = (4/3)(4/1)/(8/4)=16/70
# P(picking 4) = (4/4)(4/0)/(8/4)=1/70
# So, the probability of seeing 3 or more is 16/70 || 0.24, 
# this is our p-value, this procedure to obtain a p-value 
# is called R.A Fischer exact test.

# Two by Two tables:
# The data from this experiment can be summarized like this:
tab = matrix(c(3,1,1,3),2,2)
rownames(tab) = c("Poured Before","Poured After")
colnames(tab) = c("Guessed before","Guessed after")

# These are referred to as a two-by-two table. For each of 
# the four combinations one can get with a pair of binary variables,
# they show the observed counts for each occurrence.
# The function fisher.test performs the inference calculations above:
results = fisher.test(tab, alternative="greater")
results$p.value

# Notice that, in a way, our funding rates example is similar to the
# Lady Tasting Tea. However, in the Lady Tasting Tea example, the 
# number of blue and red beads is experimentally fixed and the 
# number of answers given for each category is also fixed. This 
# is because Fisher made sure there were four cups with milk 
# poured before tea and four cups with milk poured after and 
# the lady knew this, so the answers would also have to include 
# four befores and four afters. If this is the case, the sum 
# of the rows and the sum of the columns are fixed. This defines 
# constraints on the possible ways we can fill the two by two 
# table and also permits us to use the hypergeometric distribution.
# In general, this is not the case. Nonetheless, there is another
# approach, the Chi-squared test, which is described below.
# Imagine we have 290, 1,345, 177, 1,011 applicants, some are 
# men and some are women and some get funded, whereas others don’t. 
# We saw that the success rates for men and women were:
totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))
# Let's obtain the overall rate:
overall_rate = totals %>% summarize(overall_percent = 
                       (yes_men + yes_women) / 
                       (yes_men + 
                        yes_women +
                        no_men + 
                        no_women)) %>%
  pull(overall_percent)
# The Chi-square test can answer how often we would see the 
# success rate for men and women with the overall rate. 
# The first step is to create the two-by-two data table:
two_by_two = data.frame(
  awarded = c("yes", "no"),
  men = c(totals$yes_men, totals$no_men),
  women = c(totals$yes_women, totals$no_women)
)

# The general idea of the Chi-square test is to compare this 
# two-by-two table to what you expect to see, which would be:
men_total =totals$no_men + totals$yes_men
women_total =totals$no_women + totals$yes_women
expected_two_by_two = data.frame(
  awarded = c("yes", "no"),
  men = men_total * c(1 - overall_rate, overall_rate),
  women = women_total * c(1 - overall_rate, overall_rate)
  )

# We can see that more men than expected and fewer women than
# expected received funding. However, under the null hypothesis 
# these observations are random variables. The Chi-square test
# tells us how likely it is to see a deviation this large or 
# larger. This test uses an asymptotic result, similar to the CLT,
# related to the sums of independent binary outcomes. The R 
# function chisq.test takes a two-by-two table and returns the
# results from the test:
chi_sqr_test = two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chi_sqr_test$p.value

# The odds ratio
# An informative summary statistic associated with two-by-two
# tables is the odds ratio. Define the two variables as X=1 if 
# you are a male and 0 otherwise, and Y=1 if you are funded and 
# 0 otherwise. The odds of getting funded if you are 
# a man is defined:
# Pr(Y = 1 | X = 1) / Pr(X = 0 | Y = 1) or
odds_men = with(two_by_two, (men[1]/sum(men)) / (men[2]/sum(men)))

# and for women:
odds_women = with(two_by_two, (women[1]/sum(women)) / (women[2]/sum(women)))

# The odds ratio is the ratio for these two odds: how many
# times larger are the odds for men than for women?
odds_ratio = odds_men /odds_women

# We often see two-by-two tables written out as  
#       Awarded   Rejected
# men      a         b
# women    c         d
# the odds ratio being a * d / b * c

# Confidence intervals for the odds ratio
# Computing confidence intervals for the odds ratio is not 
# mathematically straightforward. Unlike other statistics,
# for which we can derive useful approximations of their 
# distributions, the odds ratio is not only a ratio, but a 
# ratio of ratios. Therefore, there is no simple way of using, 
# for example, the CLT.
# However, statistical theory tells us that when all four 
# entries of the two-by-two table are large enough, then the
# log of the odds ratio is approximately normal with standard error:
# sqrt(1 / a + 1 / b + 1 / c + 1/ d)
# Therefore a 95% confidence interval can be computed with:
# log(a * d / b * c) +|- qnorm(0.975) *
# sqrt(1 / a + 1 / b + 1 / c + 1/ d)
# By exponentiating these two numbers we can construct a 
# confidence interval of the odds ratio.
log_ratio = log(odds_ratio)
se = two_by_two %>%
  select(-awarded) %>%
  summarize(se = sqrt(sum(1/men)+sum(1/women))) %>%
  pull(se)
confidence_interval = log_ratio + c(-1,1) * qnorm(0.975) * se
confidence_interval
exp(confidence_interval)

# Note that 1 is not included in the confidence interval which 
# must mean that the p-value is smaller than 0.05. We can confirm
# this using:
2*(1 - pnorm(log_ratio, 0, se))

# This is a slightly different p-value than that with the
# Chi-square test. This is because we are using a different
# asymptotic approximation to the null distribution.

# Small count correction
# Note that the log odds ratio is not defined if any of the cells
# of the two-by-two table is 0. This is because if a, b, c, 
# or d is 0, the log(a * d / b * c) is either the log of 0 
# or has a 0 in the denominator. For this situation, it 
# is common practice to avoid 0s by adding 0.5 to each cell.
# This is referred to as the Haldane–Anscombe correction
# and has been shown, both in practice and theory, to work well.

# Large samples, small p-values
# As mentioned earlier, reporting only p-values is not 
# an appropriate way to report the results of data analysis. 
# In scientific journals, for example, some studies seem 
# to overemphasize p-values. Some of these studies have 
# large sample sizes and report impressively small p-values. 
# Yet when one looks closely at the results, we realize
# odds ratios are quite modest: barely bigger than 1. In 
# this case the difference may not be practically significant
# or scientifically significant.
# Note that the relationship between odds ratio and p-value 
# is not one-to-one. It depends on the sample size. So a 
# very small p-value does not necessarily mean a very large 
# odds ratio. Notice what happens to the p-value if we multiply
# our two-by-two table by 10, which does not change the odds ratio:
two_by_two_chi_q_test = two_by_two %>%
  select(-awarded) %>%
  mutate(men = men * 10, women = women * 10) %>%
  chisq.test()
two_by_two_chi_q_test$p.value
