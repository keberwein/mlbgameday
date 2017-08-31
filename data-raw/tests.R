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
 

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)
stopImplicitCluster()
rm(cl)

library(doParallel); library(foreach); library(dplyr); library(purrr); library(stringr); library(xml2); library(magrittr)

# Full season in parallel =  17 minues. Took pitchRx took 30 min. full gids w DB con.
# Full season in parallel = 24 minutes current. Faster than PitchRx but still sux.
# Full season no parallel =
urlz=make_gids(start = "2016-10-01", end = "2016-10-02", cluster = NULL)

start=Sys.time() ; print(start)
zzz= tidygameday::get_payload(start = "2016-10-01", end = "2016-10-02", cluster = "cl")
end=Sys.time()
runtime = end - start
runtime


start=Sys.time() ; print(start) #7.34 minutes
#scrape(start = "2016-04-03", end = "2016-10-02", connect = con)
prx <- pitchRx::scrape(start = "2016-10-01", end = "2016-10-02")
end = Sys.time()
runtime = end-start
runtime
