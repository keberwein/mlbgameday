# Grab new game IDs
library(pitchRx)
gids <- pitchRx::gids
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d")

new.gids <- pitchRx:::updateGids(last.date, last.date + 371)

game_ids <- unique(c(gids, new.gids))

# Add a date column
#game_ids <- as.data.frame(game_ids)
#game_ids <- rename(game_ids, gid = game_ids)
#game_ids$date_dt <- stringr::str_sub(game_ids$gid, 5, 14) %>% stringr::str_replace_all("_", "-")

rm(end, last.date, last.game, new.gids, start, time)
devtools::use_data(game_ids, overwrite = TRUE)


