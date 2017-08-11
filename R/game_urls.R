#' Create urls from game_ids.
#' @param game_id A single \code{game_id} or a list of game_ids.
#' @param cluster A named parallel cluster produced by the \code{parallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_sub
#' @importFrom purrr map_chr
#' @importFrom dplyr setdiff
#' @import doParallel
#' @export
#' @examples
#' \dontrun{
#' urls <- gameid2url(gids)
#' }
#' 

# Create urls based on game_ids
gameid2url <- function(game_id=NULL, cluster=NULL) {
    # Create an emply list to hold the results of the loop.
    datalist=gidzlist=NULL
    root <- "http://gd2.mlb.com/components/game/"
    # Loop through gids and convert them to urls.
    for(i in seq_along(game_id)) {
        league <- str_sub(game_id[i], -5, -3)
        yyyy <- str_sub(game_id[i], 5, 8)
        mm <- str_sub(game_id[i], 10, 11)
        dd <- str_sub(game_id[i], 13, 14)
        mlb_url <- paste0(root, league, "/", "year_", yyyy, "/month_", mm, "/day_", dd, "/", game_id)
        datalist[i] <- mlb_url[i]
    }
    
    # Use miniscorboard to validate urls.
    # If there are more than 1000 gids, use a parallel loop.
    minilist <- datalist %>% purrr::map_chr(~ paste0(., "/miniscoreboard.xml"))
    
    # Use foreach if parallel, otherwise use purrr::map.
    invalid=NULL
    ifelse(!is.null(cluster),
    invalid <- foreach::foreach(i = 1:length(minilist)) %dopar% {
        if(!isTRUE(tidygameday::urlTrue(minilist[i]))){
            invalid[i] <- minilist[i]
        }
    },
    for(i in 1:length(minilist)){
        if(!isTRUE(RCurl::url.exists(minilist[i]))){
            invalid[i] <- minilist[i]        
        }
    })
    
    # strip out nulls nulls.
    invalid <- Filter(Negate(is.null), invalid) %>% as.character()
    
    # Subset games not played out of minilist and out of base gids.
    minilist %<>% setdiff(invalid)
    datalist %<>% setdiff(str_sub(invalid, 1, str_length(invalid)-19))

    # Append suffix to base URLs. There is probbaly a more elegant way to do this, but this works for now...
    # Maybe go ahead and assign classes to objects here.
    inningsalllist <- datalist %>% map_chr(~ paste0(., "/inning/inning_all.xml"))
    innignshitlist <- datalist %>% map_chr(~ paste0(., "/inning/inning_hit.xml"))
    playerslist <- datalist %>% map_chr(~ paste0(., "/players.xml"))
    gameslist <- datalist %>% map_chr(~ paste0(., "/game.xml"))
    gameeventslist <- datalist %>% map_chr(~ paste0(., "/game_events.xml"))

    gidz <- c(inningsalllist, innignshitlist, minilist, playerslist, gameslist, gameeventslist)
    
    return(gidz)
}




    
