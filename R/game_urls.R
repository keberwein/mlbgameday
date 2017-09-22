#' Create urls from game_ids.
#' @param url_gids A list of gid urls formatted by the \code{make_gids()} function.
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "players",
#' "game", and "game_events".
#' @param ... additional arguments
#' @importFrom stringr str_sub str_length
#' @importFrom purrr map_chr map
#' @importFrom dplyr setdiff
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' urls <- game_urls(gids)
#' }
#' 

# Create urls based on game_ids
game_urls <- function(url_gids=NULL, dataset = NULL, ...) {
    # Make sure input is a list, so the payload function can read the output correctly.
    if(length(dataset) > 1) stop("Please specify a single data set. Due to conflicting table names, scrapes are limieted to a single set.")
    
    if(dataset=="bis_boxscore"){
        glist <- url_gids %>% purrr::map_chr(~ paste0(., "/bis_boxscore.xml")) %>%
            purrr::map(~ structure(., class = "gd_bis_boxscore"))
    }
    
    if(dataset=="game_events"){
        glist <- url_gids %>% purrr::map_chr(~ paste0(., "/game_events.xml")) %>%
            purrr::map(~ structure(., class = "gd_game_events"))
    }
    
    if(dataset=="inning_all"){
        glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_all.xml")) %>%
            purrr::map(~ structure(., class = "gd_inning_all"))
    }
    if(dataset=="inning_hit"){
        glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_hit.xml")) %>%
            purrr::map(~ structure(., class = "gd_inning_hit"))
    }
    if(dataset=="linescore"){
        glist <- url_gids %>% purrr::map_chr(~ paste0(., "/linescore.xml")) %>%
            purrr::map(~ structure(., class = "gd_linescore"))
    }
    return(glist)
}
