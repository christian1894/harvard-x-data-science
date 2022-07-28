# Variance Explained
# The bivariate normal theory also tells us that the 
# standard deviation of the conditional distribution 
# described above is:
# SD(Y∣X=x)=σY√1−ρ^2
# To see why this is intuitive, notice that without conditioning, 
# SD(Y)=σY, we are looking at the variability of all the sons. 
# But once we condition, we are only looking at the variability 
# of the sons with a tall, 72-inch, father. This group will 
# all tend to be somewhat tall so the standard deviation is reduced.
# Specifically, it is reduced to √1−ρ2=√1−0.25 = 0.87 of what 
# it was originally. We could say that father heights “explain” 
# 13% of the variability observed in son heights.

# The statement “X explains such and such percent of the 
# variability” is commonly used in academic papers. In this 
# case, this percent actually refers to the variance (the SD squared).
# So if the data is bivariate normal, the variance is reduced 
# by 1−ρ^2, so we say that X explains 1−(1−ρ^2)=ρ^2 (the correlation
# squared) of the variance.
# But it is important to remember that the “variance explained” 
# statement only makes sense when the data is approximated by a 
# bivariate normal distribution.
