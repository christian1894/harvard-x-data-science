# Understanding confounding through stratification

# A first approach is to keep HRs fixed at a certain value 
# and then examine the relationship between BB and runs. 
# As we did when we stratified fathers by rounding to the
# closest inch, here we can stratify HR per game to the 
# closest ten. We filter out the strata with few points to 
# avoid highly variable estimates:
library(tidyverse)
library(dslabs)
library(Lahman)
data("Teams")

hr_fixed_data = Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game_strata = round((HR / G), 1),
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_per_game_strata >= 0.4 & HR_per_game_strata <=1.2) 

# and then make a scatterplot for each strata:

hr_fixed_data %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_per_game_strata)

# Remember that the regression slope for predicting runs
# with BB was 0.7. Once we stratify by HR, these slopes 
# are substantially reduced:

get_slope = function(x, y) { 
  cor(x, y) * (sd(y) / sd(x))
}

hr_fixed_data %>%
  group_by(HR_per_game_strata) %>%
  summarize(slope = get_slope(BB_per_game, R_per_game))

# The slopes are reduced, but they are not 0, which indicates
# that BBs are helpful for producing runs, just not as much 
# as previously thought. In fact, the values above are 
# closer to the slope we obtained from singles, 0.45, 
# which is more consistent with our intuition. Since 
# both singles and BB get us to first base, they should 
# have about the same predictive power.

# Although our understanding of the application tells us 
# that HR cause BB but not the other way around, we can 
# still check if stratifying by BB makes the effect of BB 
# go down. To do this, we use the same code except that we 
# swap HR and BBs to get this plot:

bb_fixed_data = Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game_strata = round(BB / G,1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_per_game_strata >= 2.8 & BB_per_game_strata <= 3.9)


bb_fixed_data %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_per_game_strata)

# In this case, the slopes do not change much from the original:

bb_fixed_data %>%
  group_by(BB_per_game_strata) %>%
  summarize(slope = get_slope(HR_per_game, R_per_game))

# They are reduced a bit, which is consistent with the 
# fact that BB do in fact cause some runs.

Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G,
         R_per_game = R / G) %>%
  summarize(get_slope(HR_per_game, R_per_game))

# Regardless, it seems that if we stratify by HR, we 
# have bivariate distributions for runs versus BB. 
# Similarly, if we stratify by BB, we have approximate
# bivariate normal distributions for HR versus runs.

# Multivariable regression

# It is somewhat complex to be computing regression lines for 
# each strata. We are essentially fitting models like this:
# E[R∣BB=x1,HR=x2]=β0+β1(x2)x1+β2(x1)x2

# with the slopes for x1 changing for different values of x2
# and vice versa. But is there an easier approach?
# If we take random variability into account, the slopes 
# in the strata don’t appear to change much. If these 
# slopes are in fact the same, this implies that β1(x2) and β2(x1)
# are constants. This in turn implies that the expectation 
# of runs conditioned on HR and BB can be written like this:
# E[R∣BB=x1,HR=x2]=β0+β1x1+β2x2

# This model suggests that if the number of HR is fixed at 
# x2, we observe a linear relationship between runs 
# and BB with an intercept of β0+β2x2. Our exploratory
# data analysis suggested this. The model also 
# suggests that as the number of HR grows, the 
# intercept growth is linear as well and determined by β1x1.

# In this analysis, referred to as multivariable regression, 
# you will often hear people say that the BB slope β1
# is adjusted for the HR effect. If the model is correct 
# then confounding has been accounted for. But how do
# we estimate β1 and β2 from the data? For this, we 
# learn about linear models and least squares estimates.
