library(dslabs)
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
data(reported_heights)

dat = mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y = factor(dat$sex, c("Female", "Male"))
x = dat$type

# The type column of dat indicates whether students took classes 
# in person ("inclass") or online ("online"). What proportion of 
# the inclass group is female? What proportion of the online 
# group is female?

# In class

dat %>%
  filter(type == "inclass") %>%
  summarize(female_proportion = mean(sex == "Female"))

# Online
dat %>%
  filter(type == "online") %>%
  summarize(female_proportion = mean(sex == "Female"))

# In the course videos, height cutoffs were used to predict sex.
# Instead of height, use the type variable to predict sex. Assume
# that for each class type the students are either all male or all
# female, based on the most prevalent sex in each class type you 
# calculated in Q1. Report the accuracy of your prediction of
# sex based on type. You do not need to split the data into
# training and test sets.
# Enter your accuracy as a percentage or decimal (eg "50%" or "0.50") to 
# at least the hundredths place.

y_hat = ifelse(dat$type == "online", "Female", "Male") %>%
  factor(levels = levels(y))
mean(dat$sex == y_hat)

# Write a line of code using the table() function to show the
# confusion matrix between y_hat and y. Use the exact format table(a, b) 
# for your answer and do not name the columns and rows. Your answer
# should have exactly one space. Enter the line of code below.

table(predicted = y_hat, actual = y)

# What is the sensitivity of this prediction? You can use the sensitivity() 
# function from the caret package. Be sure that you input both your
# predicted and actual values as a factor if using this function. 
# Enter your answer as a percentage or decimal (eg "50%" or "0.50")
# to at least the hundredths place.

confusion_matrix = confusionMatrix(y_hat, reference = y)
confusion_matrix$byClass["Sensitivity"]

# What is the specificity of this prediction? You can use
# the specificity() function from the caret package.
# Enter your answer as a percentage or decimal (eg "50%" or "0.50") 
# to at least the hundredths place.

confusion_matrix$byClass["Specificity"]

# What is the prevalence (% of females) in the dat dataset defined
# above? Enter your answer as a percentage or decimal 
# (eg "50%" or "0.50") to at least the hundredths place.

confusion_matrix$byClass["Prevalence"]

# We will practice building a machine learning algorithm using a 
# new dataset, iris, that provides multiple predictors for
# us to use to train. To start, we will remove the setosa 
# species and we will focus on the versicolor and virginica
# iris species using the following code:

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# First let us create an even split of the data into train and
# test partitions using createDataPartition() from the caret 
# package.

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Next we will figure out the singular feature in the dataset that
# yields the greatest overall accuracy when predicting species. 
# You can use the code from the introduction and from Q7 to start 
# your analysis.

# Using only the train iris dataset, for each feature, perform
# a simple search to find the cutoff that produces the highest 
# accuracy, predicting virginica if greater than the cutoff and 
# versicolor otherwise. Use the seq function over the range of
# each feature by intervals of 0.1 for this search.
# Which feature produces the highest accuracy?
  

get_cutoffs = function(rows, step = 0.1) {
  min = min(rows)
  max = max(rows)
  seq(min, max, step)
}

get_best_cutoff = function(rows) {
  get_accuracy = function(cutoff) {
    y_hat = ifelse(rows > cutoff, "virginica", "versicolor") %>%
      factor(levels = levels(test$Species))
    mean(y_hat == train$Species)
  }
  
  cutoffs = get_cutoffs(rows)
  accuracy = map_dbl(cutoffs, get_accuracy)
  max_accuracy = max(accuracy)
  best_cutoff = cutoffs[which.max(accuracy)]
  as.data.frame(cbind(max_accuracy, best_cutoff))
}

sepal_length_best_cutoff = get_best_cutoff(train$Sepal.Length) %>%
  mutate(type = "sepal_length")
sepal_width_best_cutoff = get_best_cutoff(train$Sepal.Width) %>%
  mutate(type = "sepal_width")
petal_length_best_cutoff = get_best_cutoff(train$Petal.Length) %>%
  mutate(type = "petal_length")
petal_width_best_cutoff = get_best_cutoff(train$Petal.Width) %>%
  mutate(type = "petal_width")

best_cutoffs = as.data.frame(bind_rows(sepal_length_best_cutoff, 
                        sepal_width_best_cutoff, 
                        petal_length_best_cutoff, 
                        petal_width_best_cutoff)) %>%
  arrange(desc(max_accuracy))
most_accurate_cutoff = best_cutoffs %>%
  filter(max_accuracy == max(max_accuracy))

# For the feature selected in Q8, use the smart cutoff value from 
# the training data to calculate overall accuracy in the test data.
# What is the overall accuracy?

cutoff = most_accurate_cutoff %>% pull(best_cutoff) 
get_accuracy = function(cutoff) {
  y_hat = ifelse(test$Petal.Width > cutoff, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
}
accuracy = get_accuracy(cutoff)

# Notice that we had an overall accuracy greater than 90% in the 
# training data, but the overall accuracy was lower in the test data. 
# This can happen often if we overtrain. In fact, it could be the case 
# that a single feature is not the best choice. For example, a 
# combination of features might be optimal. Using a single feature 
# and optimizing the cutoff as we did on our training data can
# lead to overfitting.

# To consider which other features could be helpful to add, 
# we can repeat the analysis from Q8.
# Which feature produces the second highest accuracy?

best_cutoffs[2,]

# Now we will perform some exploratory data analysis on the data.

plot(iris, pch=21, bg=iris$Species)

# Notice that Petal.Length and Petal.Width in combination could
# potentially be more information than either feature alone.

# Optimize the the cutoffs for Petal.Length and Petal.Width 
# separately in the train dataset by using the seq function 
# with increments of 0.1. Then, report the overall accuracy when 
# applied to the test dataset by creating a rule that predicts
# virginica if Petal.Length is greater than the length cutoff AND
# Petal.Width is greater than the width cutoff, and versicolor otherwise.
# What is the overall accuracy for the test data now?

petal_width_cutoff = best_cutoffs %>% 
  filter(type == "petal_width") %>%
  pull(best_cutoff)

petal_length_cutoff = best_cutoffs %>% 
  filter(type == "petal_length") %>%
  pull(best_cutoff)

get_accuracy = function() {
  y_hat = ifelse(test$Petal.Width > petal_width_cutoff & 
                   test$Petal.Length > petal_length_cutoff, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
}
accuracy = get_accuracy()
accuracy
  