library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
# vector length
length(male)
length(female)
# display percentiles
percentiles_vector = seq(0.1, 0.9, 0.2)
male_percentiles = quantile(male, percentiles_vector)
female_percentiles = quantile(female, percentiles_vector)
# dataframe of quantiles
headers = c("female", "male")
df = data.frame(male_percentiles, female_percentiles)
