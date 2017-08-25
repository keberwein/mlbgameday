#' Create urls from game_ids.
#' @param url_gids A list of gid urls formatted by the \code{make_gids()} function.
#' @param cluster A named parallel cluster produced by the \code{doParallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_sub str_length
#' @importFrom purrr map_chr
#' @importFrom dplyr setdiff
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' urls <- game_urls(gids)
#' }
#' 

# Create urls based on game_ids
game_urls <- function(url_gids=NULL, cluster=NULL) {
    # Make sure input is a list, so the payload function can read the output correctly.
    

    # Append suffix to base URLs. There is probbaly a more elegant way to do this, but this works for now...
    # Should try to use map2() here.
    # This whole thing could be one map2() loop with if() statements...maybe.
    # Make final urls and assign the correct classes.
    inningsalllist <- url_gids %>% purrr::map(~ paste0(., "/inning/inning_all.xml")) %>%
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
    
}
