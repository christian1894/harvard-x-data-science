# Case study: is it a 2 or a 7?

# In the simple example above, we only had one predictor. We actually
# do not consider these machine learning challenges, which are
# characterized by cases with many predictors. Let’s go back to 
# the digits example in which we had 784 predictors. For illustrative
# purposes, we will start by simplifying this problem to one with two
# predictors and two classes. Specifically, we define the challenge as 
# building an algorithm that can determine if a digit is a 2 or 7 from
# the predictors. We are not quite ready to build algorithms with 
# 784 predictors, so we will extract two simple predictors from the 
# 784: the proportion of dark pixels that are in the upper left quadrant
# (X1) and the lower right quadrant (X2). We then select a random sample
# of 1,000 digits, 500 in the training set and 500 in the test set.
# We provide this dataset in the dslabs package:
library(tidyverse)
library(dslabs)
data("mnist_27")

# We can explore the data by plotting the two predictors and 
# using colors to denote the labels:

mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# We can immediately see some patterns. For example, if X1
# (the upper left panel) is very large, then the digit is probably
# a 7. Also, for smaller values of X1, the 2s appear to be in the 
# mid range values of X2.

# To illustrate how to interpret X1 and X2, we include four example images.
# On the left are the original images of the two digits with the largest
# and smallest values for X1 and on the right we have the images 
# corresponding to the largest and smallest values of X2:

if(!exists("mnist")) mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p1 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_1")

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p2 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_2")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# We can start getting a sense for why these predictors are useful, but 
# also why the problem will be somewhat challenging.

# We haven’t really learned any algorithms yet, so let’s try building 
# an algorithm using regression. The model is simply:
# p(x1,x2)=Pr(Y=1∣X1=x1,X2=x2)=β0+β1x1+β2x2

# We fit it like this:

fit = mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)


# We can now build a decision rule based on the estimate of ^p(x1,x2):

library(caret)
p_hat = predict(fit, newdata = mnist_27$test)
y_hat = factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

# We get an accuracy well above 50%. Not bad for our first try. But can we
# do better? Because we constructed the mnist_27 example and we had at 
# our disposal 60,000 digits in just the MNIST dataset, we used 
# this to build the true conditional distribution p(x1,x2). Keep in mind
# that this is something we don’t have access to in practice, but we 
# include it in this example because it permits the comparison of ^p(x1,x2) 
# to the true p(x1,x2). This comparison teaches us the limitations
# of different algorithms. Let’s do that here. We have stored the 
# true p(x1,x2) in the mnist_27 object and can plot the image using 
# the ggplot2 function geom_raster(). We choose better colors and
# use the stat_contour function to draw a curve that separates pairs 
# (x1,x2) for which p(x1,x2)>0.5 and pairs for which p(x1,x2)<0.5:

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# Above you see a plot of the true p(x1,x2). To start understanding the 
# limitations of regression here, first note that with regression ^p(x1,x2)
# has to be a plane, and as a result the boundary defined by the decision 
# rule is given by: ^p(x1,x2)=0.5, which implies the boundary can’t be 
# anything other than a straight line:
# ^β0+^β1x1+^β2x2=0.5⟹^β0+^β1x1+^β2x2=0.5⟹x2=(0.5−^β0)/^β2−^β1/^β2x1
# Note that for this boundary, x2 is a linear function of x1. This 
# implies that our regression approach has no chance of capturing 
# the non-linear nature of the true p(x1,x2). Below is a visual
# representation of ^p(x1,x2). We used the squish function from the 
# scales package to constrain estimates to be between 0 and 1. We 
# can see where the mistakes were made by also showing the data and 
# the boundary. They mainly come from low values x1 that have either
# high or low value of x2. Regression can’t catch this.

p_hat = predict(fit, newdata = mnist_27$true_p)
p_hat = scales::squish(p_hat, c(0, 1))
p1 = mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 
p2 = mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 
gridExtra::grid.arrange(p1, p2, ncol = 2)

# We need something more flexible: a method that permits estimates with 
# shapes other than a plane. We are going to learn a few new algorithms
# based on different ideas and concepts. But what they all have in
# common is that they permit more flexible approaches. We will start 
# by describing nearest neighbor and kernel approaches. To introduce 
# the concepts behinds these approaches, we will again start with a 
# simple one-dimensional example and describe the concept of smoothing.
