# Combinations and permutations

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# Generating a deck of cards
suits = c("Diamonds", "Clubs", "Hearts", "Spades")
numbers = c("Ace", "Deuce", "Three", "Four", "Five", "Six",
             "Seven", "Eight", "Nine", "Ten", "Jack", "Queen",
             "King")
deck = expand.grid(value = numbers, suit = suits)
deck = paste(deck$value, deck$suit)

# probability of drawing a 3(proportion)
threes = paste("Three", suits)
mean(deck %in% threes)

# generating all possible phone numbers without repeats
library(gtools)
all_phone_numbers <- permutations(10, 7, v = 0:9)
n = nrow(all_phone_numbers)
selected_numbers_indexes = sample(n, 5) # select 5 random numbers
all_phone_numbers[selected_numbers_indexes,]

# Probability of drawing a second king given that
# one king is drawn

hands = permutations(52, 2, v = deck) # order matters in permutations
all_first_cards = hands[,1]
all_second_cards = hands[,2]
kings = paste("King", suits)
sum(all_first_cards %in% kings)

sum(all_first_cards %in% kings & all_second_cards %in% kings) / sum(all_first_cards %in% kings)

# Probability of a natural 21 in blackjack

aces = paste("Ace", suits)
ten_valued_cards = c("King", "Queen", "Jack", "Ten")
ten_valued_cards = expand.grid(value = ten_valued_cards, suit = suits)
ten_valued_cards = paste(ten_valued_cards$value, ten_valued_cards$suit)

all_hands = combinations(52, 2, v = deck) # order does not matter in combinations

# probability of getting blackjack, ace in either draw
mean((all_hands[,1] %in% aces & all_hands[,2] %in% ten_valued_cards) | mean(all_hands[,2] %in% aces & all_hands[,1] %in% ten_valued_cards))

# monte carlo simulation of a blackjack draw
hand_sample = sample(deck, 2)
B = 10000
events = replicate(B, {
  hand = sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% ten_valued_cards) | (hand[1] %in% ten_valued_cards & hand[2] %in% aces)
})
mean(events)
