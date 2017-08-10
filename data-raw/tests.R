library(pitchRx)
library(RPostgreSQL)

# Load Postgres driver.
drv <- dbDriver("PostgreSQL")
# Attempt to connect.
con <- dbConnect(drv, host= "localhost", dbname = "pitchrx", user="kriseberwein", password="pissoff")

# Started with a fresh R shesh
start = Sys.time()
scrape(start = "2016-04-03", end = "2016-10-02", connect = con)
end = Sys.time()
runtime = end-start

# Find out if these gids were cached in data or pulled from miniscore.
# Final run time 30.86305 mins with normal cpu and network overhead at house.
# 
# 
# 
# 


startgid <- tidygameday::game_ids[24602]
endgid <- tidygameday::game_ids[27050]
mygids <- tidygameday::game_ids[24602:27050]

urlgids <- tidygameday::gameid2url(mygids)