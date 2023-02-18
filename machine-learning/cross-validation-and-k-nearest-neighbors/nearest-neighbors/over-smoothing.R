# Over-smoothing
# Although not as badly as with the previous examples, we saw that with k=5
# we also over-trained. Hence, we should consider a larger k. Let’s try, 
# as an example, a much larger number: k=401.

knn_fit_401 = knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 = predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

# This turns out to be similar to regression: 
p_hat = predict(fit_lm, newdata = mnist_27$true_p)
p_hat = scales::squish(p_hat, c(0, 1))
p1 = plot_cond_prob(p_hat) +
  ggtitle("Regression")
p2 = plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")

grid.arrange(p1, p2, nrow=1)

# This size of k is so large that it does not permit enough flexibility. 
# We call this over-smoothing.
