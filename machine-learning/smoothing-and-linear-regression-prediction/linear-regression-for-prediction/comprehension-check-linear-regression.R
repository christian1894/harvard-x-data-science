# Comprehension Check: Linear Regression
# Create a data set using the following code:

library(tidyverse)
library(caret)

set.seed(1)
n = 100
Sigma = 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat = MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above and calculate 
# the mean and standard deviation of the combined models. First, set
# the seed to 1 again. Then, within a replicate() loop, (1) partition 
# the dataset into test and training sets with p = 0.5 and using dat$y
# to generate your indices, (2) train a linear model predicting y from x,
# (3) generate predictions on the test set, and (4) calculate the RMSE 
# of that model. Then, report the mean and standard deviation (SD) of 
# the RMSEs from all 100 models.

# Report all answers to at least 3 significant digits.
set.seed(1)
options(digits = 3)

number_of_models = 100
rmse = replicate(number_of_models, {
  test_index = createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set = dat %>% slice(-test_index)
  test_set = dat %>% slice(test_index)
  fit = lm(y ~ x, data = train_set)
  y_hat = predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y) ^2))
  
})
# Mean:
mean(rmse)

# Standard deviation (SD):
sd(rmse)

# Now we will repeat the exercise above but using larger datasets. Write
# a function that takes a size n, then (1) builds a dataset using the code 
# provided at the top of Q1 but with n observations instead of 100 and 
# without the set.seed(1), (2) runs the replicate() loop that you wrote
# to answer Q1, which builds 100 linear models and returns a vector of
# RMSEs, and (3) calculates the mean and standard deviation of the 100 RMSEs.

# Set the seed to 1 and then use sapply() or map() to apply your new 
# function to n <- c(100, 500, 1000, 5000, 10000).

# Note: You only need to set the seed once before running your 
# function; do not set a seed within your function. Also be sure 
# to use sapply() or map() as you will get different answers 
# |running the simulations individually due to setting the seed.

generate_dataset = function(n) {
  Sigma = 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat = MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  dat
}

models_rmse = function(dataset) {
  rmse = replicate(number_of_models, {
    test_index = createDataPartition(dataset$y, times = 1, p = 0.5, list = FALSE)
    train_set = dataset %>% slice(-test_index)
    test_set = dataset %>% slice(test_index)
    fit = lm(y ~ x, data = train_set)
    y_hat = predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y) ^2))
  })
}

get_model_metrics = function(dataset_size) {
  dataset = generate_dataset(dataset_size)
  rmse = models_rmse(dataset)
  metrics = as.data.frame(cbind(mean(rmse),sd(rmse)))
  colnames(metrics) = c("mean", "sd")
  metrics
}

dataset_sizes = c(100, 500, 1000, 5000, 10000)

set.seed(1)
results = sapply(dataset_sizes, get_model_metrics)
results = as.data.frame(results)
colnames(results) = c("100", "500", "1000", "5000", "10000")
results

# On average, the RMSE does not change much as n gets larger, but 
# the variability of the RMSE decreases. 

# Now repeat the exercise from Q1, this time making the correlation 
# between x and y larger, as in the following code:
  
set.seed(1)
n = 100
Sigma = 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat = MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

# Note what happens to RMSE - set the seed to 1 as before.

rmse = models_rmse(dat)

# mean
mean(rmse)

# sd
sd(rmse)

# When we increase the correlation between x and y, x has more predictive 
# power and thus provides a better estimate of y. 

# Create a data set using the following code.

set.seed(1)
Sigma = matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat = MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Note that y is correlated with both x_1 and x_2 but the two predictors
# are independent of each other, as seen by cor(dat).

# Set the seed to 1, then use the caret package to partition into 
# test and training sets with p = 0.5. Compare the RMSE when using 
# just x_1, just x_2 and both x_1 and x_2. Train a single linear model 
# for each (not 100 like in the previous questions).

set.seed(1)
test_index = createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set = dat %>% slice(-test_index)
test_set = dat %>% slice(test_index)

fit_x_1 = lm(y ~ x_1, data = train_set)
y_hat = predict(fit_x_1, test_set)
rmse_x_1 = sqrt(mean((y_hat - test_set$y) ^2))

fit_x_2 = lm(y ~ x_2, data = train_set)
y_hat = predict(fit_x_2, test_set)
rmse_x_2 = sqrt(mean((y_hat - test_set$y) ^2))

fit_x_1_x_2 = lm(y ~ x_1 + x_2, data = train_set)
y_hat = predict(fit_x_1_x_2, test_set)
rmse_x_1_x_2 = sqrt(mean((y_hat - test_set$y) ^2))

# Which of the three models performs the best (has the lowest RMSE)?
rmses = c(rmse_x_1, rmse_x_2, rmse_x_1_x_2)
rmses
rmses[which.min(rmses)]

# Repeat the exercise from Q6 but now create an example in which x_1
# and x_2 are highly correlated.  

set.seed(1)
Sigma = matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat = MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Set the seed to 1, then use the caret package to partition into 
# a test and training set of equal size. Compare the RMSE when using 
# just x_1, just x_2, and both x_1 and x_2.

set.seed(1)
test_index = createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set = dat %>% slice(-test_index)
test_set = dat %>% slice(test_index)

fit_x_1 = lm(y ~ x_1, data = train_set)
y_hat = predict(fit_x_1, test_set)
rmse_x_1 = sqrt(mean((y_hat - test_set$y) ^2))

fit_x_2 = lm(y ~ x_2, data = train_set)
y_hat = predict(fit_x_2, test_set)
rmse_x_2 = sqrt(mean((y_hat - test_set$y) ^2))

fit_x_1_x_2 = lm(y ~ x_1 + x_2, data = train_set)
y_hat = predict(fit_x_1_x_2, test_set)
rmse_x_1_x_2 = sqrt(mean((y_hat - test_set$y) ^2))

rmses = c(rmse_x_1, rmse_x_2, rmse_x_1_x_2)
rmses

# Compare the results from Q6 and Q8. What can you conclude?

# Adding extra predictors can improve RMSE substantially, but 
# not when the added predictors are highly correlated with other predictors. 
