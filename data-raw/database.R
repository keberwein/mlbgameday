
library(DBI)
library(mlbgameday)

# SQL lite



dplyr::tbl(con, "po")


urlz <- search_gids(start = "2015-04-05", end = "2015-10-01")

innings_df <- mlbgameday::get_payload(game_ids = urlz)









