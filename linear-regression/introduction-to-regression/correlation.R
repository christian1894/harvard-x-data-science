# Up to this point, this book has focused mainly on single 
# variables. However, in data science applications, it is very 
# common to be interested in the relationship between two or 
# more variables. For instance, in Chapter 19 we will use 
# a data-driven approach that examines the relationship 
# between player statistics and success to guide the 
# building of a baseball team with a limited budget. 
# Before delving into this more complex example, we 
# introduce necessary concepts needed to understand 
# regression using a simpler illustration. We actually 
# use the dataset from which regression was born.

# The example is from genetics. Francis Galton studied 
# the variation and heredity of human traits. Among 
# many other traits, Galton collected and studied height
# data from families to try to understand heredity. While 
# doing this, he developed the concepts of correlation and
# regression, as well as a connection to pairs of data that 
# follow a normal distribution. Of course, at the time this
# data was collected our knowledge of genetics was quite 
# limited compared to what we know today. A very specific 
# question Galton tried to answer was: how well can we predict
# a child’s height based on the parents’ height? The technique 
# he developed to answer this question, regression, can also 
# be applied to our baseball question. Regression can be applied 
# in many other circumstances as well.

# Case study: is height hereditary?
# We have access to Galton’s family height data through the 
# HistData package. This data contains heights on several 
# dozen families: mothers, fathers, daughters, and sons. 
# To imitate Galton’s analysis, we will create a dataset 
# with the heights of fathers and a randomly selected son 
# of each family:

library(tidyverse)
library(HistData)
library(dslabs)
data("GaltonFamilies")
set.seed(1983)

galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Suppose we were asked to summarize the father and son data. 
# Since both distributions are well approximated by the normal
# distribution, we could use the two averages and two standard 
# deviations as summaries:
galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

# However, this summary fails to describe an important 
# characteristic of the data: the trend that the taller 
# the father, the taller the son.
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# We will learn that the correlation coefficient is an informative 
# summary of how two variables move together and then see how this 
# can be used to predict one variable using the other.
