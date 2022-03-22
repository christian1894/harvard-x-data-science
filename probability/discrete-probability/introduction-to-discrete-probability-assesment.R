# One ball will be drawn at random from a box containing: 
# 3 cyan balls, 5 magenta balls, and 7 yellow balls. 

box = rep(c("cyan", "magenta", "yellow"), times = c(3, 5, 7))

# What is the probability that the ball will be cyan?
probability_cyan = mean(box == "cyan")

# What is the probability that the ball will not be cyan?
probability_not_cyan = mean(box != "cyan")

# What is the probability that the first draw is cyan and 
# that the second draw is not cyan?
box_with_one_cyan_draw = rep(c("cyan", "magenta", "yellow"), times = c(2, 5, 7))
probability_not_cyan_drawn_box = mean(box_with_one_cyan_draw != "cyan")
probability_cyan * probability_not_cyan_drawn_box

# Now repeat the experiment, but this time, after taking the 
# first draw and recording the color, return it back to the box
#and shake the box. We call this sampling with replacement.

# What is the probability that the first draw is cyan and 
# that the second draw is not cyan?
probability_cyan * probability_not_cyan

