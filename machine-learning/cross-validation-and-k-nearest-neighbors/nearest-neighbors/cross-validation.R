# Cross validation

# In this chapter we introduce cross validation, one of the most
# important ideas in machine learning. Here we focus on the conceptual 
# and mathematical aspects. We will describe how to implement cross
# validation in practice with the caret package later, in Section 30.2 
# in the next chapter. To motivate the concept, we will use the two
# predictor digits data presented in Section 27.8 and introduce,
# for the first time, an actual machine learning algorithm: k-nearest 
# neighbors (kNN).

# Motivation with k-nearest neighbors
# Let’s start by loading the data and showing a plot of the predictors 
# with outcome represented with color.

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

# We will use these data to estimate the conditional probability function
# p(x1,x2)=Pr(Y=1∣X1=x1,X2=x2).
# as defined in Section 28.4. With k-nearest neighbors (kNN) we estimate 
# p(x1,x2) in a similar way to bin smoothing. However, as we will see, 
# kNN is easier to adapt to multiple dimensions. First we define the 
# distance between all observations based on the features. Then, for 
# any point (x1,x2) for which we want an estimate of p(x1,x2), we look
# for the k nearest points to (x1,x2) and then take an average of the
# 0s and 1s associated with these points. We refer to the set of points 
# used to compute the average as the neighborhood. Due to the connection 
# we described earlier between conditional expectations and conditional
# probabilities, this gives us a ^p(x1,x2), just like the bin smoother 
# gave us an estimate of a trend. As with bin smoothers, we can control
# the flexibility of our estimate, in this case through the k parameter:
# larger ks result in smoother estimates, while smaller ks result in
# more flexible and more wiggly estimates.
# To implement the algorithm, we can use the knn3 function
# from the caret package. Looking at the help file for this package,
# we see that we can call it in one of two ways. We will use 
# the first in which we specify a formula and a data frame. The
# data frame contains all the data to be used. The formula has
# the form outcome ~ predictor_1 + predictor_2 + predictor_3 and so 
# on. Therefore, we would type y ~ x_1 + x_2. If we are going to use 
# all the predictors, we can use the . like this y ~ .. The final call 
# looks like this:

library(caret)
knn_fit = knn3(y ~ ., data = mnist_27$train)

# For this function, we also need to pick a parameter: the number of
# neighbors to include. Let’s start with the default k=5.
knn_fit = knn3(y ~ ., data = mnist_27$train, k = 5)

# In this case, since our dataset is balanced and we care just as much 
# about sensitivity as we do about specificity, we will use accuracy to
# quantify performance.
# The predict function for knn produces a probability for each class.
# We keep the probability of being a 7 as the estimate ^p(x1,x2)

y_hat_knn = predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

# In Section 27.8 we used linear regression to generate an estimate.
fit_lm = mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm = predict(fit_lm, mnist_27$test)
y_hat_lm = factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

# And we see that kNN, with the default parameter, already beats regression.
# To see why this is the case, we will plot ^p(x1,x2)
# and compare it to the true conditional probability p(x1,x2):

plot_cond_prob = function(p_hat=NULL){
  tmp = mnist_27$true_p
  if(!is.null(p_hat)){
    tmp = mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

p1 = plot_cond_prob() + ggtitle("True conditional probability")
p2 = plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
library(gridExtra)
grid.arrange(p2, p1, nrow=1)

# We see that kNN better adapts to the non-linear shape of p(x1,x2).
# However, our estimate has some islands of blue in the red area, 
# which intuitively does not make much sense. This is due to what we
# call over-training. We describe over-training in detail below. Over-training
# is the reason that we have higher accuracy in the train set compared
# to the test set:
y_hat_knn = predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn = predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
