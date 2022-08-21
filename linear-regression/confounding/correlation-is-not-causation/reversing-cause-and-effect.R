# Reversing cause and effect

library(tidyverse)
library(dslabs)
library(HistData)
library(broom)
data("GaltonFamilies")

galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  rename(son = childHeight)

# Another way association is confused with causation is 
# when the cause and effect are reversed. An example of 
# this is claiming that tutoring makes students perform 
# worse because they test lower than peers that are not 
# tutored. In this case, the tutoring is not causing 
# the low test scores, but the other way around.

# A form of this claim actually made it into an op-ed in the
# New York Times titled Parental Involvement Is Overrated.
# Consider this quote from the article:

# When we examined whether regular help with homework had
# a positive impact on children’s academic performance, 
# we were quite startled by what we found. Regardless of 
# a family’s social class, racial or ethnic background, 
# or a child’s grade level, consistent homework help 
# almost never improved test scores or grades… Even 
# more surprising to us was that when parents regularly 
# helped with homework, kids usually performed worse.

# A very likely possibility is that the children needing 
# regular parental help, receive this help because they 
# don’t perform well in school.

# We can easily construct an example of cause and effect 
# reversal using the father and son height data. If we 
# fit the model: Xi=β0+β1yi+εi,i=1,…,N
# to the father and son height data, with Xi the father
# height and yi the son height, we do get a statistically 
# significant result. We use the galton_heights dataset
# defined in Section 18.1:

galton_heights %>% summarize(tidy(lm(father ~ son)))

# The model fits the data very well. If we look at the
# mathematical formulation of the model above, 
# it could easily be incorrectly interpreted so as 
# to suggest that the son being tall caused the father 
# to be tall. But given what we know about genetics 
# and biology, we know it’s the other way around. 
# The model is technically correct. The estimates 
# and p-values were obtained correctly as well. What 
#is wrong here is the interpretation.
