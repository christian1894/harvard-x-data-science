# Over-training
# Over-training is at its worst when we set k=1. With k=1, the estimate
# for each (x1,x2) in the training set is obtained with just the y 
# corresponding to that point. In this case, if the (x1,x2)
# are unique, we will obtain perfect accuracy in the training set 
# because each point is used to predict itself. Remember that if
# the predictors are not unique and have different outcomes for
# at least one set of predictors, then it is impossible to predict perfectly.
# Here we fit a kNN model with k=1:
knn_fit_1 = knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 = predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall[["Accuracy"]]

# However, the test set accuracy is actually worse than regression:
y_hat_knn_1 = predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]

# We can see the over-fitting problem in this figure. 
tmp = mnist_27$true_p
tmp$knn = predict(knn_fit_1, newdata =  mnist_27$true_p)[,2]
p1 = tmp %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y),
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 = tmp %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

# The black curves denote the decision rule boundaries. The estimate ^p(x1,x2)
# follows the training data too closely (left). You can see that in the
# training set, boundaries have been drawn to perfectly surround a single 
# red point in a sea of blue. Because most points (x1,x2) are unique,
# the prediction is either 1 or 0 and the prediction for that point is
# the associated label. However, once we introduce the training set 
# (right), we see that many of these small islands now have the opposite
# color and we end up making several incorrect predictions.
