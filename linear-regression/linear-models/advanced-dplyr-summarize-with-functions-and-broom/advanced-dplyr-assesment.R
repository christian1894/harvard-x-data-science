# Assessment: Advanced dplyr

library(Lahman)
library(dslabs)
library(tidyverse)
library(broom)
data("Teams")

# You want to take the tibble dat, which we used in the 
# video on the advanced dplyr, and run the linear model 
# R ~ BB for each strata of HR. Then you want to add 
# three new columns to your grouped tibble: the 
# coefficient, standard error, and p-value for the 
# BB term in the model.

# You’ve already written the function get_slope(), shown below.

get_slope = function(data) {
  fit = lm(R ~ BB, data = data)
  sum.fit = summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# As a reminder, the tibble dat is defined as follows: 

dat = Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))

# You want to know whether the relationship between 
# home runs and runs per game varies by baseball 
# league. You create the following dataset:

dat = Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>%
  group_by(lgID) %>%
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = TRUE)) %>%
  filter(term == "HR")

# We have investigated the relationship between fathers'
# heights and sons' heights. But what about other 
# parent-child relationships? Does one parent's height
# have a stronger association with child height? How does 
# the child's gender affect this relationship in heights? 
# Are any differences that we observe statistically significant?

# The galton dataset is a sample of one male and one 
# female child from each family in the GaltonFamilies 
# dataset. The pair column denotes whether the pair is 
# father and daughter, father and son, mother and 
# daughter, or mother and son.

# Create the galton dataset using the code below:

library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton = GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

# Group by pair and summarize the number of 
# observations in each group.

pairs = galton %>%
  group_by(pair) %>%
  summarize(n = n())

# How many father-daughter pairs are in the dataset?
pairs %>%
  filter(pair == "father_daughter") %>%
  pull(n)

# How many mother-son pairs are in the dataset?
pairs %>%
  filter(pair == "mother_son") %>%
  pull(n)

# Calculate the correlation coefficients for 
# fathers and daughters, fathers and sons, 
# mothers and daughters and mothers and sons.

correlations = galton %>%
  group_by(pair) %>%
  summarize(coefficient = cor(parentHeight, childHeight)) 

# Which pair has the strongest correlation in heights?
  
correlations %>%
  filter(coefficient == max(coefficient)) %>%
  pull(pair)

# Which pair has the weakest correlation in heights?
correlations %>%
  filter(coefficient == min(coefficient)) %>%
  pull(pair)

# Use lm() and the broom package to fit regression lines 
# for each parent-child pair type. Compute the least 
# squares estimates, standard errors, confidence 
# intervals and p-values for the parentHeight 
# coefficient for each pair.

fit = galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) 

# What is the estimate of the father-daughter coefficient?
fit %>%
  filter(term == "parentHeight" &
         pair == "father_daughter") %>%
  pull(estimate)

# For every 1-inch increase in mother's height,
# how many inches does the typical son's height increase?
fit %>%
  filter(term == "parentHeight" &
           pair == "mother_son") %>%
  pull(estimate)

# Which sets of parent-child heights are significantly 
# correlated at a p-value cut off of 0.05?

galton %>%  
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < 0.05) %>%
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
