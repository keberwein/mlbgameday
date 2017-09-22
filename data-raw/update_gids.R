library(dplyr);library(stringr); library(purrr)
library(mlbgameday); library(doParallel); library(foreach)

# Get current game_ids and check for the last date.
gids <- mlbgameday::game_ids
last.game <- sort(gids$gameday_link)[length(gids$gameday_link)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d") %>% + 1

# Set up a cluster to make this faster.
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)

# use make_gids function to get gids not yet in the internal database.
new_gids <- mlbgameday::get_payload(start = as.character(last.date), end = as.character(Sys.Date()-1), dataset = "linescore")

# Format the linescore dataframe to match the gids df.
new_game_ids <- new_gids$game %>% subset(status = "Final", 
                                         select = c("gameday_link", "venue", "home_team_city", "home_team_name",
                                                                      "away_team_city", "away_team_name", "game_type", "venue_id",
                                                                      "home_team_id", "away_team_id")) %>%
    mutate(gameday_link = paste0("gid_", gameday_link)) %>%
    # Add dates to the df just so we can sort it.
    mlbgameday::gid_date() %>% arrange(date_dt)

# Combine and re-save the dataframe.
game_ids <- bind_rows(new_game_ids, gids)

# Remove cluster.
stopImplicitCluster()
rm(cl)

devtools::use_data(game_ids, overwrite = TRUE)





