#' Create urls from game_ids.
#' @param url_gids A list of gid urls formatted by the \code{make_gids()} function.
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "players",
#' "game", and "game_events".
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
game_urls <- function(url_gids=NULL, dataset = "inning_all", ...) {
    # Make sure input is a list, so the payload function can read the output correctly.
    if(length(dataset) > 1) stop("Please specify a single data set. Due to conflicting table names, scrapes are limieted to a single set.")
    
    if(dataset=="inning_all"){
        glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_all.xml"))
    }
    if(dataset=="inning_hit"){
        glist <- datalist %>% purrr::map_chr(~ paste0(., "/inning/inning_hit.xml"))
    }
    if(dataset=="players"){
        glist <- datalist %>% purrr::map_chr(~ paste0(., "/players.xml"))
    }
    if(dataset=="game"){
        glist <- datalist %>% purrr::map_chr(~ paste0(., "/game.xml"))
    }
    if(dataset=="game_events"){
        glist <- datalist %>% purrr::map_chr(~ paste0(., "/game_events.xml"))
    }
    return(glist)
}
