#' Create urls from game_ids.
#' @param game_id A single \code{game_id} or a list of game_ids.
#' @param cluster A named parallel cluster produced by the \code{doParallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_sub str_length
#' @importFrom purrr map_chr
#' @importFrom dplyr setdiff
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' urls <- gameid2url(gids)
#' }
#' 

# Create urls based on game_ids
game_urls <- function(game_id=NULL, cluster=NULL) {
    # Create an emply list to hold the results of the loop.
    datalist=i=str_length=NULL
    root <- "http://gd2.mlb.com/components/game/"
    # Loop through gids and convert them to urls.
    datalist <- purrr::map_chr(game_id, function(i){
        output <- paste0(root, stringr::str_sub(i, -5, -3), "/year_", stringr::str_sub(i, 5, 8), "/month_", 
                         stringr::str_sub(i, 10, 11), "/day_", stringr::str_sub(i, 13, 14), "/", i)
    })
    
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
    invalid <- purrr::map(minilist, function(i){
        if(!isTRUE(tidygameday::urlTrue(i))){
            output <- i
        }
    }))
    
    # strip out nulls nulls.
    invalid <- Filter(Negate(is.null), invalid) %>% as.character()
    
    # Subset games not played out of minilist and out of base gids.
    minilist %<>% dplyr::setdiff(invalid)
    datalist %<>% dplyr::setdiff(stringr::str_sub(invalid, 1, stringr::str_length(invalid)-19))

    # Append suffix to base URLs. There is probbaly a more elegant way to do this, but this works for now...
    # Should try to use map2() here.
    # This whole thing could be one map2() loop with if() statements...maybe.
    # Make final urls and assign the correct classes.
    inningsalllist <- datalist %>% purrr::map(~ paste0(., "/inning/inning_all.xml")) %>%
        purrr::map(~ structure(., class = "inning_all"))
    
    #inningshitlist <- datalist %>% purrr::map_chr(~ paste0(., "/inning/inning_hit.xml")) %>%
    #    purrr::map(~ structure(., class = "inning_hit"))
    
    #playerslist <- datalist %>% purrr::map_chr(~ paste0(., "/players.xml")) %>%
    #    purrr::map(~ structure(., class = "players"))
    
    #gameslist <- datalist %>% purrr::map_chr(~ paste0(., "/game.xml")) %>%
    #    purrr::map(~ structure(., class = "game"))
    
    #gameeventslist <- datalist %>% purrr::map_chr(~ paste0(., "/game_events.xml")) %>%
    #    purrr::map(~ structure(., class = "game_events"))
    
    #gidz <- c(inningsalllist, inningshitlist, minilist, playerslist, gameslist, gameeventslist)
    
    gidz <- inningsalllist
    
    
    return(gidz)
}
