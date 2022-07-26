# The regression line
# If we are predicting a random variable Y knowing the value 
# of another X=x using a regression line, then we predict that 
# for every standard deviation, σX, that x increases above the
# average μX, Y increase ρ standard deviations σY above the average
# μY with ρ the correlation between X and Y. The formula for the
# regression is therefore: (Y−μY/σY)=ρ(x−μX/σX)

# We can rewrite it like this: Y=μY+ρ(x−μX/σX)σY

# If there is perfect correlation, the regression line predicts 
# an increase that is the same number of SDs. If there is 0 
# correlation, then we don’t use x at all for the prediction and
# simply predict the average μY. 
# For values between 0 and 1, the prediction is somewhere in
# between. If the correlation is negative, we predict a 
# reduction instead of an increase.
  
# Note that if the correlation is positive and lower than 1, our
# prediction is closer, in standard units, to the average 
# height than the value used to predict, x, is to the
# average of the xs. This is why we call it regression: the 
# son regresses to the average height. In fact, the title 
# of Galton’s paper was: Regression toward mediocrity in 
# hereditary stature. To add regression lines to plots, 
# we will need the above formula in the form:
# y=b+mx with slope m=ρ(σy/σx) and intercept b=μy−mμx
# Here we add the regression line to the original data:

mu_x = mean(galton_heights$father)
mu_y = mean(galton_heights$son)
s_x = sd(galton_heights$father)
s_y = sd(galton_heights$son)
r = cor(galton_heights$father, galton_heights$son)

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y / s_x, intercept = mu_y - r * s_y / s_x * mu_x)

# The regression formula implies that if we first standardize 
# the variables, that is subtract the average and divide by 
# the standard deviation, then the regression line has intercept 
# 0 and slope equal to the correlation ρ. You can make same plot, 
# but using standard units like this:

galton_heights %>%
  mutate(father = scale(father), son = scale(son)) %>%
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r, intercept = 0)
