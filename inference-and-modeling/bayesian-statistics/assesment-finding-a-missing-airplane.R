options(digits = 3)

# Finding a missing airplane
# A search team is tasked with finding a crashed plane. 
# Their initial data suggest the plane is found 
# somewhere within one of four areas with the 
# following probabilities:
Pr_A = 0.2
Pr_B = 0.6
Pr_C = 0.15
Pr_D = 0.05
Pr_miss_plane_in_area = 0.1
Pr_not_found_if_plane_in_different_area = 1

# The team can search one area per day and will always 
# search the area with the highest probability. When 
# the team searches an area, there is a 90% chance 
# that they find a plane if one is present and a 
# 10% chance that they overlook the plane. (There is 
# always a 100% chance of not finding a plane if one 
# is not present in the area.)

# On day 1, the team will search area B. 
# The probability that the plane is in area B is 0.6.
# What is the probability the plane is not in B?
1 - Pr_B

# What is the probability the plane is in B but is not found?
# Pr(plane not found | Pr_B)
Pr_B * Pr_miss_plane_in_area

# What is Pr(plane not found in B),
# the probability the plane is not found in B on day 1?
# Pr(plane_not_found | Pr_B or plane_in_other_area)
Pr_not_found_in_B = (Pr_miss_plane_in_area * Pr_B) + (1 - Pr_B)

# Suppose the plane will not be found on day 1. 
# What is the equation for
# Pr(plane_in_B | plane_not_found_in_B), the posterior 
# probability that the plane is in area B given that it 
# is not found in area B on day 1?

# Pr(plane_not_found_in_B | plane_in_B) * 
# Pr(plane_not_found_in_B) / Pr(plane_in_B)

# Use Bayes' Theorem to calculate the posterior 
# probabilities of finding the plane in each of the 4 
# grid locations. Recall that area B will be searched 
# on day 1.

# What is the posterior probability that the
# plane is in area B given that it is not found on day 1?
# Pr(plane_in_B | plane_not_found_on_day_1):
# Pr(plane_not_found_in_day_1 | plane_in_B) * 
# Pr(plane_in_B) / Pr(plane_not_found_in_day_1)
Pr_plane_in_B_if_not_found_on_day_1 = 
  (Pr_miss_plane_in_area * Pr_B) / Pr_not_found_in_B

# What is the posterior probability that the plane is in 
# area C given that it is not found on day 1?
# Pr(plane_in_C | plane_not_found_on_day_1):
# Pr(plane_not_found_in_day_1 | plane_in_C) * 
# Pr(plane_in_C) / Pr(plane_not_found_in_day_1)
Pr_plane_in_C_if_not_found_on_day_1 = 
  (Pr_not_found_if_plane_in_different_area * Pr_C) / 
  Pr_not_found_in_B

# Which area has the highest posterior probability 
# and should be searched on day 2?
Pr_plane_in_A_if_not_found_on_day_1 = 
  (Pr_not_found_if_plane_in_different_area * Pr_A) / 
  Pr_not_found_in_B
Pr_plane_in_D_if_not_found_on_day_1 = 
  (Pr_not_found_if_plane_in_different_area * Pr_D) / 
  Pr_not_found_in_B
results = c(Pr_plane_in_A_if_not_found_on_day_1,
            Pr_plane_in_B_if_not_found_on_day_1,
            Pr_plane_in_C_if_not_found_on_day_1,
            Pr_plane_in_D_if_not_found_on_day_1)
area_names = c("A", "B", "C", "D")
results_dataframe = data.frame(area_names, results)
results_dataframe[which.max(results_dataframe$results),]

# Before the search begins, you have been asked to 
# report the probability that you find the plane 
# within two days.
# What is the probability of finding the plane on the first day?
# Pr(plane_found | plane_in_B)
Pr_B * (1 - Pr_miss_plane_in_area)

# What is the probability that the plane is not found on the
# first day but is found on the second day?
# On day 2, you will search the region that had the 
# highest posterior probability in question 3. Make sure 
# you multiply the probability of a successful search on 
# day 2 by the probability that the plane is not found 
# on day 1.
Pr_found_in_A = (Pr_plane_in_A_if_not_found_on_day_1 * (1 - Pr_miss_plane_in_area))
Pr_found_on_second_day = Pr_not_found_in_B * Pr_found_in_A
  
# What is the probability that the plane is found within 2 days?
# Pr(found_on_day_1) +
# Pr(found_on_day_2 | plane_on_A_and_not_found_on_day_1)
Pr_found_in_B + Pr_found_on_second_day
