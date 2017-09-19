library(purrr); library(doParallel); library(foreach); library(mlbgameday); 
library(stringr); library(xml2); library(dplyr); library(tidyr)
# Get a list of the current gids and fromat them to grab the "/players.xml" file for each gid.
gidenv <- environment()
data(game_ids, package = "mlbgameday", envir = gidenv)
root <- paste0("http://gd2.mlb.com/components/game/mlb/")
# Format the urls.
glist <- game_ids %>% 
    purrr::map_chr(~ paste0(root, "year_", stringr::str_sub(., 5, 8), 
                            "/month_", stringr::str_sub(., 10,11), 
                            "/day_", stringr::str_sub(., 13, 14),"/", ., "/players.xml")) %>% as.list()


# NOTE: We only need to select the new gids that have been added. Any others would be doing double-work.
glist = glist[1:27070]



# Use parallel here, otherwise it would take forever.
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)

players <- foreach::foreach(i = seq_along(glist)) %dopar% {
                            file <- tryCatch(xml2::read_xml(glist[[i]]), error=function(e) NULL)
                            if(!is.null(file)){
                                player_nodes <- xml2::xml_find_all(file, "./team/player")
                                player_df <- purrr::map_dfr(player_nodes, function(x) {
                                    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                    out
                                })
                            }
                        }


stopImplicitCluster()
rm(cl)

new_players <- dplyr::bind_rows(players) %>% select("id", "first", "last") %>% 
    tidyr::unite(full_name, c("first", "last"), sep = " ") %>%
    filter(!is.na(full_name)) %>% unique()
    

rm(players, game_ids, glist, playergids, players); gc()

# Now we've got a data frame with uniqe player ids. We need to pull the current player data and do a left join.

current_players <- mlbgameday::player_ids

# A duplicate column is created on the join due to slight changes in name spellings. OK just to drop it.
player_ids <- dplyr::left_join(current_players, new_players, by = "id") %>% 
    select("id", "full_name.x") %>% rename(full_name=full_name.x)

# The list returns duplicate ids because players sometimes change thier names. Example, "B.J" Upton began calling himself "Melvin".
# This is a crude work-around for that, only selecting one name.
player_ids <- player_ids[!duplicated(player_ids$id),]


# Save new ids
devtools::use_data(player_ids, overwrite = TRUE)

