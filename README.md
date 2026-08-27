# Liar's Dice

An interactive Liar's Dice game in base R. Handles an arbitrary number of players and
dice per player, prompts each player for a bid at the console, resolves challenges by
counting face values across all hands, and eliminates dice until one player remains.

Written to get comfortable with R's control flow and `sample()` before using the same
Monte Carlo approach on real problems.

## Running it

```r
source("LiarsDice.R")
play_liars_dice(num_players = 4, num_dice = 5)
```

Bids are entered as `quantity value` — e.g. `3 4` claims there are at least three 4s
on the table.

## License

MIT
