library(mlbgameday); library(dplyr)

team_ids <- mlbgameday::game_ids %>% select(home_team_city, home_team_name, home_team_id) %>% unique()
devtools::use_data(team_ids, overwrite = TRUE)


venue_ids <- mlbgameday::game_ids %>% select(venue, venue_id) %>% unique()
devtools::use_data(venue_ids, overwrite = TRUE)
