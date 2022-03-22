# Monty hall problem
B = 10000
stick_with_first_door_events = replicate(B, {
  doors = as.character(1:3)
  prizes = sample(c("car", "goat", "goat"))
  prize_door = doors[prizes == "car"]
  choice = sample(doors, 1)
  shown_door = sample(doors[!doors %in% c(choice, prize_door)], 1)
  final_choice = choice # contestant does not switch chosen door
  final_choice == prize_door
})
mean(stick_with_first_door_events)

# contestant switches for shown door
switch_door_events = replicate(B, {
  doors = as.character(1:3)
  prizes = sample(c("car", "goat", "goat"))
  prize_door = doors[prizes == "car"]
  choice = sample(doors, 1)
  shown_door = sample(doors[!doors %in% c(choice, prize_door)], 1)
  final_choice = doors[!doors %in% c(choice, shown_door)] # contestant switch chosen door
  final_choice == prize_door
})
mean(switch_door_events)