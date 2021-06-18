library(dslabs)
data(heights)

# Empirical Cumulative distribution function plot
male_heights = heights$height[heights$sex == "Male"]
male_heights_ecdf = ecdf(male_heights)
plot(male_heights_ecdf,
     do.points = FALSE,
     verticals = TRUE,
     xlab = 'Height',
     ylab = '',
     main = 'Male Heights',
     panel.first = grid())

# Histogram
male_heights_histogram = hist(male_heights,
     breaks = 30, plot = FALSE)
plot(male_heights_histogram,
     col = "lightblue",
     xlab = "Male Heights",
     main = "Male Heights Histogram",
     panel.first = grid())

# Density plot
density = density(male_heights)
plot(density, 
     col = "lightblue",
     xlab = "Male Heights",
     main = "Male Heights Density Plot",
     panel.first = grid())
polygon(density, col="lightblue") 

