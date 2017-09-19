# Grab new game IDs
library(dplyr)
library(stringr)
library(purrr)
library(mlbgameday)

# Get current game_ids and check for the last date.
gids <- mlbgameday::game_ids
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d") %>% + 1

# use make_gids function to get gids not yet in the internal database.
new_gids <- mlbgameday::make_gids(start = as.character(last.date), end = as.character(Sys.Date()-1))
# make_gids returns the gids in a url format, so strip these back down to basic format.
new_gids <- str_sub(new_gids, 66, -16)
# combine the lists.
game_ids <- unique(c(gids, new_gids))

rm(end, last.date, last.game, new.gids, start, time)
devtools::use_data(game_ids, overwrite = TRUE)


