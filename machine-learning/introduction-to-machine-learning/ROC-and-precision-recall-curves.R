# ROC and precision-recall curves
# When comparing the two methods (guessing versus using a height cutoff), 
# we looked at accuracy and F1. The second method clearly outperformed 
# the first. However, while we considered several cutoffs for the
# second method, for the first we only considered one approach: 
# guessing with equal probability. Note that guessing Male with 
# higher probability would give us higher accuracy due to the
# bias in the sample:

library(tidyverse)
library(caret)
library(dslabs)
library(ggrepel)
library(ggplot2)
data(heights)

x = heights$height
y = heights$sex

set.seed(2007)
test_index = createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set = heights[test_index,]
train_set = heights[-test_index,]

p = 0.9
n = length(test_index)
y_hat = sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1 - p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# But, as described above, this would come at the cost of lower 
# sensitivity. The curves we describe in this section will help us see this.
# Remember that for each of these parameters, we can get a different 
# sensitivity and specificity. For this reason, a very common 
# approach to evaluating methods is to compare them graphically 
# by plotting both. A widely used plot that does this is the 
# receiver operating characteristic (ROC) curve. If you are
# wondering where this name comes from, you can consult the
# ROC Wikipedia page. The ROC curve plots sensitivity (TPR)
# versus 1 - specificity or the false positive rate (FPR). Here
# we compute the TPR and FPR needed for different
# probabilities of guessing male:

probs = seq(0, 1, length.out = 10)
guessing = map_df(probs, function (p) {
  y_hat = sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1 - p)) %>%
    factor(levels = c("Male", "Female"))
  list(method = "Guessing",
       cutoff = round(p,1),
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# We can use similar code to compute these values for our our second
# approach. By plotting both curves together, we are able to compare
# sensitivity for different values of specificity:

cutoffs = c(50, seq(60, 75), 80)
height_cutoff = map_df(cutoffs, function (cutoff) {
  y_hat = ifelse(cutoff > test_set$height, "Male", "Female") %>%
    factor(levels = c("Male", "Female"))
  list(method = "Height Cutoff",
       cutoff = cutoff,
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

as.data.frame(bind_rows(guessing, height_cutoff)) %>%
  ggplot(aes(TPR, FPR, color = method, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.02, nudge_y = -0.02, show.legend = FALSE)

# We can see that we obtain higher sensitivity with this approach 
# for all values of specificity, which implies it is in fact a better 
# method. Note that ROC curves for guessing always fall on the 
# identiy line. Also note that when making ROC curves, it is often 
# nice to add the cutoff associated with each point.
# The packages pROC and plotROC are useful for generating these plots.
# ROC curves have one weakness and it is that neither of the
# measures plotted depends on prevalence. In cases in which
# prevalence matters, we may instead make a precision-recall plot. 
# The idea is similar, but we instead plot precision against recall:

male_guessing = map_df(probs, function (p) {
  y_hat = sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1 - p)) %>%
    factor(levels = c("Male", "Female"))
  list(method = "Guessing",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

male_height_cutoff = map_df(cutoffs, function(cutoff){
  y_hat = ifelse(test_set$height > cutoff, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

female_guessing = map_df(probs, function (p) {
  y_hat = sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1 - p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

female_height_cutoff = map_df(cutoffs, function(cutoff){
  y_hat = ifelse(test_set$height > cutoff, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

male_metrics = bind_rows(male_guessing, male_height_cutoff) %>%
  mutate(Positive = "Y = 1 if Male") 

female_metrics = bind_rows(female_guessing, female_height_cutoff) %>%
  mutate(Positive = "Y = 1 if Female")

as.data.frame(bind_rows(male_metrics, female_metrics)) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~Positive)


# From this plot we immediately see that the precision of guessing
# is not high. This is because the prevalence is low. We also see 
# that if we change positives to mean Male instead of Female, the
# ROC curve remains the same, but the precision recall plot changes.
