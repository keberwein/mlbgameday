library(doParallel); library(foreach); library(dplyr); library(purrr); library(stringr); library(xml2); library(magrittr)

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
pload <- mlbgameday::get_payload(inningsalllist)
end = Sys.time()
runtime = end-start
 

no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)  
registerDoParallel(cl)
stopImplicitCluster()
rm(cl)


# Full season in parallel =  8.3 minues. Took pitchRx took 30 min. full gids w DB con.
# Full season no parallel = 41.02
# Full season still slower than pitchrx. This may be due to the pitchrx DB con and garbage collection.
game_ids <- search_gids(start = "2016-04-01", end = "2016-05-01", game_type = "r")
zzz= mlbgameday::get_payload(game_ids = game_ids, dataset = "inning_all")

# linescore not working. It's in the dopar loop. Checkt urlz.
start=Sys.time() ; print(start)
innings_df <- mlbgameday::get_payload(start = "2015-09-03", end = "2015-09-04", dataset = "game")
end=Sys.time()
runtime = end - start
runtime



# database test. two seasons
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

print(pryr::mem_used())
start=Sys.time() ; print(start)
#con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "gameday.sqlite3")
innings_df <- mlbgameday::get_payload(start = "2016-08-01", end = "2016-08-02", db_con = NULL)
end=Sys.time()
runtime = end - start
runtime
print(pryr::mem_used())


stopImplicitCluster()
rm(cl)

DBI::dbDisconnect(con)
rm(con)


start=Sys.time() ; print(start)
#scrape(start = "2016-04-03", end = "2016-10-02", connect = con)
prx <- pitchRx::scrape(start = "2016-08-01", end = "2016-08-02")
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
#

library(openWAR)
gd <- gameday()

ds <- openWAR::getData()
