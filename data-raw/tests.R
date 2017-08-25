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

# Final run time 30.86305 mins with normal cpu and network overhead at house. Carson has those gids in-cache.
# Looks like Carson's default is only innings_all, but functionality for the others.
# 
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


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)
stopImplicitCluster()
rm(cl)

library(doParallel); library(foreach); library(dplyr); library(purrr); library(stringr); library(xml2); library(magrittr)

# 200 full games takes 7 minutes. fresh R environ. no parallel. / parallel =  1.3 minutes.
# Full season 17 minues in parallel:) pitchRx took 30 min. full gids w DB con.
# Need to test this out of parallel and test the non-parallel map function.
# This works with no cluster, but cluster turns it into an emply list. May have to return the foreach as a single object.

start=Sys.time()

basegids <- make_gids(start = "2017-06-01", end = "2017-08-22", cluster = "cl")


url <- game_urls(basegids, cluster = "cl")

start=Sys.time() ; print(start)
zzz= tidygameday::get_payload(url, cluster='cl')
end=Sys.time()
runtime = end - start
runtime



