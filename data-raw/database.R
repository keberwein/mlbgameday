library(dplyr)
library(dbplyr)
library(DBI)
library(mlbgameday)

# SQL lite

my_db <- src_sqlite("gameday.sqlite3", create = TRUE)

innings_df <- mlbgameday::get_payload(start = "2015-09-03", end = "2015-09-04")

copy_to(my_db$con, innings_df$atbat, temporary = FALSE)

tomlin <- tbl(my_db$con, "games$atbat") %>% filter(pitcher_name == "Josh Tomlin")







                                   
