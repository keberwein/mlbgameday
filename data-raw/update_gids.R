# Grab new game IDs
library(pitchRx)
gids <- pitchRx::gids
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d")

start = Sys.time() 
new.gids <- pitchRx:::updateGids(last.date, last.date + 371)
end = Sys.time()
time= start-end

gids <- unique(c(gids, new.gids))
rm(end, last.date, last.game, new.gids, start, time)
devtools::use_data(gids, overwrite = TRUE)