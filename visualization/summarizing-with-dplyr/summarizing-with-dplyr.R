library(dplyr)
library(NHANES)
data(NHANES)

# How to ignore NA's

library(dslabs)
data(na_example)
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

# 20-29 year old females

tab = NHANES %>% 
  filter(Gender == "female" & AgeDecade == " 20-29")

# average and standard deviation of systolic blood pressure,
# which are stored in the BPSysAve variable

ref <- NHANES %>% 
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm = TRUE))

# average blood pressure for 20-29 year old females

ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  .$average

# Min and Max BPSysAve of 20-29 year old females

NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% 
  summarize(minbp = min(BPSysAve, na.rm = TRUE), 
            maxbp = max(BPSysAve, na.rm = TRUE))

# average and standard deviation of systolic blood pressure
# for females for each age group separately

NHANES %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE),
            standard_deviation = sd(BPSysAve, na.rm = TRUE))

# average and standard deviation of systolic blood pressure
# for males for each age group separately

NHANES %>%
  filter(Gender == "male") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE),
            standard_deviation = sd(BPSysAve, na.rm = TRUE))

# average and standard deviation of systolic blood pressure
# for each gender and age group

NHANES %>%
  group_by(AgeDecade, Gender) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE),
            standard_deviation = sd(BPSysAve, na.rm = TRUE))

# systolic blood pressure across values of the Race1 
# variable for males between the ages of 40-49

NHANES %>%
  filter(AgeDecade == " 40-49" & Gender == "male") %>%
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE),
            standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>%
  arrange(average)
