library(pitchRx)
library(RPostgreSQL)

# Load Postgres driver.
drv <- dbDriver("PostgreSQL")
# Attempt to connect.
con <- dbConnect(drv, host= "localhost", dbname = "pitchrx", user="kriseberwein", password="pissoff")

# Started with a fresh R shesh
start=Sys.time() ; print(start)
scrape(start = "2016-04-03", end = "2016-10-02", connect = con)
#pitchRx::scrape(start = "2016-09-03", end = "2016-10-02", connect = con)
end = Sys.time()
runtime = end-start
runtime
# Final run time 30.86305 mins with normal cpu and network overhead at house. Carson has those gids in-cache.
# Looks like Carson's default is only innings_all, but functionality for the others.
# 
# 

startgid <- tidygameday::game_ids[24602]
endgid <- tidygameday::game_ids[27050]
mygids <- tidygameday::game_ids$gid[24602:27050]
shortgids <- tidygameday::game_ids[24602:24800]


start = Sys.time()
library(doParallel)  
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)
urlgids <- tidygameday::game_urls(mygids, cluster='cl')
end = Sys.time()
runtime = end-start
runtime


start = Sys.time()
pload <- tidygameday::get_payload(inningsalllist)
end = Sys.time()
runtime = end-start
 

no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)  
registerDoParallel(cl)
stopImplicitCluster()
rm(cl)

library(doParallel); library(foreach); library(dplyr); library(purrr); library(stringr); library(xml2); library(magrittr)

# Full season in parallel =  8.3 minues. Took pitchRx took 30 min. full gids w DB con.
# Full season no parallel = 41.02
# Full season still slower than pitchrx. This may be due to the pitchrx DB con and garbage collection.
game_ids <- search_gids(team = "indians", start = "2016-05-01", end = "2016-05-01", game_type = "r")
zzz= mlbgameday::get_payload(game_ids = game_ids, dataset = "inning_all")

# linescore not working. It's in the dopar loop. Checkt urlz.
start=Sys.time() ; print(start)
zzz= mlbgameday::get_payload(start = "2015-09-03", end = "2015-09-04", dataset = "game_events")
end=Sys.time()
runtime = end - start
runtime


urlz <- make_gids(start = "2015-09-03", end = "2015-09-04", dataset = "inning_hit")


start=Sys.time() ; print(start)
#scrape(start = "2016-04-03", end = "2016-10-02", connect = con)
prx <- pitchRx::scrape(start = "2015-09-03", end = "2015-09-10")
end = Sys.time()
runtime = end-start
runtime

## 1 Day
#tidy 24.5 clean environ 
#pitchrx: 28.8 clean eviron
#
## 1 Week
#tidy 1.90
#pitchrx 1.23 min.
#
## 2 weeks / pitchrx 200 game limit
#tidy 3.40 min.
#pitchrx 2.42 min. limit 200 games
