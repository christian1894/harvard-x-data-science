library(HistData)
data(Galton)
x <- Galton$child
# average and median(happen to be similar)
mean(x)
median(x)
# standard deviation and median absolute deviation(similar again)
sd(x)
mad(x)
# these summaries are similar since the data is approximated 
# by a normal distribution

# simulate data input error
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
# average increase
mean(x_with_error) - mean(x)
# standard deviation increase
sd(x_with_error) - sd(x)
# median increase(little since 
# median is considered a robust summary)
median(x_with_error) - median(x)
# median absolute deviation increase(little since 
# median  absolute deviation is considered a robust summary)
mad(x_with_error) - mad(x)
# these are considered robust summaries since outliers
# have little effect on them

# mean is prone to vary because of outliers
error_avg <- function(k){
  x[1] = k
  mean(x)
}
error_avg(10000)
error_avg(-10000)


