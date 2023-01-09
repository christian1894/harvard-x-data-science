# The Confusion Matrix

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

x = heights$height
y = heights$sex

set.seed(2007)
test_index = createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set = heights[test_index,]
train_set = heights[-test_index,]

best_cutoff = 64
y_hat = ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat = factor(y_hat)
mean(y_hat == test_set$sex)

# The prediction rule we developed in the previous section predicts 
# Male if the student is taller than 64 inches. Given that the
# average female is about 64 inches, this prediction rule seems
# wrong. What happened? If a student is the height of the average
# female, shouldn’t we predict Female? Generally speaking, 
# overall accuracy can be a deceptive measure. To see this,
# we will start by constructing what is referred to as the
# confusion matrix, which basically tabulates each combination 
# of prediction and actual value. We can do this in
# R using the function table:

table(predicted = y_hat, actual = test_set$sex)

# If we study this table closely, it reveals a problem. If we 
# compute the accuracy separately for each sex, we get:

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

# There is an imbalance in the accuracy for males and females: too many 
# females are predicted to be male. We are calling almost half of the 
# females male! How can our overall accuracy be so high then? This
# is because the prevalence of males in this dataset is high. 
# These heights were collected from three data sciences courses, 
# two of which had more males enrolled:

mean(y == "Male")

# So when computing overall accuracy, the high percentage of mistakes
# made for females is outweighed by the gains in correct calls for men. 
# This can actually be a big problem in machine learning. If your
# training data is biased in some way, you are likely to develop 
# algorithms that are biased as well. The fact that we used a test set 
# does not matter because it is also derived from the original biased 
# dataset. This is one of the reasons we look at metrics other than 
# overall accuracy when evaluating a machine learning algorithm.

# There are several metrics that we can use to evaluate an algorithm
# in a way that prevalence does not cloud our assessment, and these
# can all be derived from the confusion matrix. A general improvement 
# to using overall accuracy is to study sensitivity and specificity separately.
