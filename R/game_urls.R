#' Create urls from game_ids.
#' @param game_id A single \code{game_id} or a list of game_ids
#' @param ... additional arguments
#' @importFrom stringr str_sub
#' @importFrom purrr map_chr
#' @importFrom dplyr setdiff
#' @export
#' @examples
#' \dontrun{
#' urls <- gameid2url(gids)
#' }
#' 

# Create urls based on game_ids
gameid2url <- function(game_id=NULL) {
    # Create an emply list to hold the results of the loop.
    datalist=gidzlist=NULL
    # Loop through gids and convert them to urls.
    for(i in seq_along(game_id)) {
    league <- str_sub(game_id[i], -5, -3)
    yyyy <- str_sub(game_id[i], 5, 8)
    mm <- str_sub(game_id[i], 10, 11)
    dd <- str_sub(game_id[i], 13, 14)
    mlb_url <- paste0(root, league, "/", "year_", yyyy, "/month_", mm, "/day_", dd, "/", game_id)
    datalist[i] <- mlb_url[i]
    }
    
    #game_id[i] %>% 
    #    purrr::map_chr(~ paste0("day_", str_sub(game_id[i], 13, 14), "/", .)) %>%
    #    purrr::map_chr(~ paste0("month_", str_sub(game_id[i], 10, 11), "/", .)) %>%
    #    purrr::map_chr(~ paste0("year_", str_sub(game_id[i], 5, 8), "/", .)) %>%
    #    purrr::map_chr(~ paste0(str_sub(game_id[i], -5, -3), "/", .)) %>%
    #    purrr::map_chr(~ paste0("http://gd2.mlb.com/components/game/", .))

    # Grab miniscoreboard first, so we an find any games that weren't played.
    minilist <- datalist %>% map_chr(~ paste0(., "/miniscoreboard.xml"))
    
    
    #### This validation loop takes WAAAAYYYY too long. Need to figure out another solution.
    #z = minilist %>% purrr::map_chr(~ tidypitch::validURL(.))
    
    # Validate urls for miniscoreboard.
    gamez=validurlz=invalid=NULL
    for(i in 1:length(minilist)){
        if(!isTRUE(RCurl::url.exists(minilist[i]))){
            invalid[i] <- minilist[i]        
        }
    }
    
    
    # Strip out any NAs.
    invalid %<>% na.omit() %>% as.character()
    
    # Subset games not played out of minilist and out of base gids.
    minilist %<>% setdiff(invalid)
    datalist %<>% setdiff(str_sub(invalid, 1, str_length(invalid)-19))

    # Append suffix to base URLs. There is probbaly a more elegant way to do this, but this works for now...
    inningsalllist <- datalist %>% map_chr(~ paste0(., "/inning/inning_all.xml"))
    innignshitlist <- datalist %>% map_chr(~ paste0(., "/inning/inning_hit.xml"))
    playerslist <- datalist %>% map_chr(~ paste0(., "/players.xml"))
    gameslist <- datalist %>% map_chr(~ paste0(., "/game.xml"))
    gameeventslist <- datalist %>% map_chr(~ paste0(., "/game_events.xml"))

    gidz <- c(inningsalllist, innignshitlist, minilist, playerslist, gameslist, gameeventslist)
    
    return(gidz)
}




    
