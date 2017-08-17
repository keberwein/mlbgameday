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
shortgids <- tidygameday::game_ids[24602:24800]


start = Sys.time()
library(doParallel)  
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)
urlgids <- tidygameday::game_urls(shortgids, cluster=NULL)
end = Sys.time()
runtime = end-start
runtime


start = Sys.time()
pload <- tidygameday::get_payload(inningsalllist)
end = Sys.time()
runtime = end-start


no_cores <- detectCores() - 2  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores, type="FORK")
stopImplicitCluster()
rm(cl)

library(doParallel); library(foreach); library(dplyr); library(purrr); library(stringr); library(xml2); library(magrittr)

# 200 innin_all.xml takes 31 seconds. fresh R environ. no parallel. / parallel = 16 sec.
# Full season inning_all 5 minues in parallel:) pitchRx took 30 min. full gids w DB con.
# Full season inning_all no parallel 23 minutes. No gid parsing. Might be slower than PitchRx.
start=Sys.time()
zzz= tidygameday::get_payload(gidz, cluster="cl")
end=Sys.time()
runtime = end - start

# Pitchrx does the same games in 1.12 minutes (full games, not just inning_all)
start=Sys.time()
zzzpitchrx=pitchRx::scrape(game.ids=game_id)
end=Sys.time()
time = end - start

