# A restaurant manager wants to advertise that his lunch special
# offers enough choices to eat different meals every day of 
# the year. He doesn't think his current special actually 
# allows that number of choices, but wants to change his
# special if needed to allow at least 365 choices.

# A meal at the restaurant includes 1 entree, 2 sides, 
# and 1 drink. He currently offers a choice of 1 entree
# from a list of 6 options, a choice of 2 different sides
# from a list of 6 options, and a choice of 1 drink from a
# list of 2 options.

# How many meal combinations are possible with the current menu?
  
entree_options = 6
sides_options = nrow(combinations(6, 2))
drink_options = 2
entree_options * sides_options * drink_options

# The manager has one additional drink he could add to the special.
# How many combinations are possible if he expands his original 
# special to 3 drink options?

drink_options = 3
entree_options * sides_options * drink_options

# The manager decides to add the third drink but needs to 
# expand the number of options. The manager would prefer not 
# to change his menu further and wants to know if he can meet his goal
# by letting customers choose more sides.

# How many meal combinations are there if customers can choose from 
# 6 entrees, 3 drinks, and select 3 sides from the current 6 options?

entree_options = 6
sides_options = nrow(combinations(6, 3))
drink_options = 3
entree_options * sides_options * drink_options

# Write a function that takes a number of entree choices and returns 
# the number of meal combinations possible given that number of entree options, 
# 3 drink choices, and a selection of 2 sides from 6 options.

get_meal_combinations = function(entree_options) {
  drink_options = 3
  sides_options = nrow(combinations(6, 2))
  entree_options * sides_options * drink_options
}

# Use sapply() to apply the function to entree option counts ranging 
# from 1 to 12.
# What is the minimum number of entree options required in
# order to generate more than 365 combinations?
number_of_entrees = 1:12
all_meal_options = sapply(number_of_entrees, get_meal_combinations)
min(number_of_entrees[all_meal_options > 365])

# The manager isn't sure he can afford to put that many entree
# choices on the lunch menu and thinks it would be cheaper for
# him to expand the number of sides. He wants to know how many 
# sides he would have to offer to meet his goal of at least 365
# combinations.

# Write a function that takes a number of side choices and returns 
# the number of meal combinations possible given 6 entree choices, 
# 3 drink choices, and a selection of 2 sides from the 
# specified number of side choices.

get_meal_combinations = function(number_of_sides) {
  drink_options = 3
  entree_options = 6
  sides_options = nrow(combinations(number_of_sides, 2))
  entree_options * sides_options * drink_options
}
number_of_sides = 2:12
all_meal_options = sapply(number_of_sides, get_meal_combinations)
data.frame(number_of_sides, all_meal_options) %>% 
  filter(all_meal_options > 365) %>% min(.$number_of_sides)
