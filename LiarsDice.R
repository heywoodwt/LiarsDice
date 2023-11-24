# Function to roll dice
roll_dice <- function(num_dice) {
  return(sample(1:6, num_dice, replace = TRUE))
}

# Function to count the number of specific face value in dice
count_dice <- function(dice, value) {
  return(sum(dice == value))
}

# Function to play Liar's Dice
play_liars_dice <- function(num_players, num_dice) {
  # Initialize dice for each player
  player_dice <- matrix(NA, nrow = num_players, ncol = num_dice)
  for (i in 1:num_players) {
    player_dice[i,] <- roll_dice(num_dice)
  }
  
  # Game loop
  while (TRUE) {
    for (player in 1:num_players) {
      # Display current dice for each player
      cat(paste("Player", player, "dice:", paste(player_dice[player,], collapse = " "), "\n"))
      
      # Prompt for bid
      bid <- as.numeric(unlist(strsplit(readline(paste("Player", player, "enter bid (e.g., '3 4'): ")), " ")))
      bid_value <- bid[2]
      bid_quantity <- bid[1]
      
      # Check if bid is valid
      if (bid_value < 1 || bid_value > 6 || bid_quantity < 1) {
        cat("Invalid bid. Try again.\n")
        next
      }
      
      # Calculate total quantity of bid value among all players
      total_count <- sum(apply(player_dice, 1, function(x) count_dice(x, bid_value)))
      total_count <- count_dice(player_dice[player,], bid_value)  # Exclude own dice
      
      # Determine if bid is valid
      if (bid_quantity > num_players * num_dice || bid_quantity <= total_count) {
        cat("Bid is invalid. Must be higher than previous bid. Try again.\n")
        next
      }
      
      # Prompt for challenge or new bid
      challenge <- as.numeric(readline("Challenge previous bid? (1 for challenge, 0 for new bid): "))
      
      # Challenge previous bid
      if (challenge == 1) {
        actual_count <- total_count + count_dice(player_dice[player,], bid_value)
        if (actual_count >= bid_quantity) {
          cat(paste("Challenge failed! Player", player, "loses a die.\n"))
          player_dice[player, sample(which(!is.na(player_dice[player,])), 1)] <- NA
        } else {
          cat("Challenge successful! Previous player loses a die.\n")
          prev_player <- player - 1
          if (prev_player == 0) {
            prev_player <- num_players
          }
          player_dice[prev_player, sample(which(!is.na(player_dice[prev_player,])), 1)] <- NA
        }
        if (sum(!is.na(player_dice[player,])) == 0) {
          cat("Player", player, "wins!\n")
          return()
        }
      }
    }
  }
}

# Start the game with 4 players and 5 dice each
play_liars_dice(4, 5)
