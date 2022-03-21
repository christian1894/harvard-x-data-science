library(dslabs)
data(heights)
# get Data types, analyze categorical vs numeric
names(heights)
# get number of unique values
x <- heights$height
length(unique(x))
# frequency table
tab = table(x)
# number of values reported once, not appropiate as ordinal data
tab_data_frame = data.frame(tab)
sum(tab_data_frame$Freq == 1)