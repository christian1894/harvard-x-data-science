# Monte carlo simulation

beads = rep(c("blue", "red"), times = c(3, 2))

# take 1 bead at random
sample(beads, 1)

B = 10000 # number of times to execute simulation
events = replicate(B, sample(beads, 1))
events_table = table(events)
prop.table(events_table) # similar to beads proportion
prop.table(table(beads))
