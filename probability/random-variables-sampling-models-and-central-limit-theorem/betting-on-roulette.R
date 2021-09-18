# A casino offers a House Special bet on roulette, which is a 
# bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets.
# The bet pays out 6 to 1. In other words, a losing bet 
# yields -$1 and a successful bet yields $6. A gambler 
# wants to know the chance of losing money if he places 
# 500 bets on the roulette House Special.

p_win = 5 / 38
win = 6
loss = -1
n = 500

# What is the expected value of the payout for one bet?
mu = (win * p_win) + (loss * (1 - p_win))

# What is the standard error of the payout for one bet?
sigma = abs(loss - win) * sqrt(p_win * (1 -  p_win))

# What is the expected value of the average payout over 500 bets?
mu

# What is the standard error of the average payout over 500 bets?
sigma/sqrt(n)

# What is the standard error of the sum of 500 bets?
sqrt(n) * sigma

# Use pnorm() with the expected value of the sum and 
# standard error of the sum to calculate the probability
# of losing money over 500 bets
pnorm(0, mu * n, sqrt(n) * sigma)





