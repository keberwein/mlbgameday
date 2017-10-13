#' Create urls from game_ids.
#' @param url_gids A list of gid urls formatted by the \code{make_gids()} function.
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "players",
#' "game", and "game_events".
#' @param ... additional arguments
#' @importFrom stringr str_sub str_length
#' @importFrom purrr map_chr
#' @export
#' @examples
#' gids <- make_gids(start = "2016-06-01", end = "2016-06-01", dataset="inning_all")
#' urls <- game_urls(gids)
#' 
#' 

# Create urls based on game_ids
game_urls <- function(url_gids=NULL, dataset = NULL, ...) {
    if(is.null(dataset)) dataset <- "inning_all"
    # Make sure input is a list, so the payload function can read the output correctly.
    if(length(dataset) > 1) stop("Please specify a single data set. Due to conflicting table names, scrapes are limieted to a single set.")
    
    if(dataset=="bis_boxscore") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/bis_boxscore.xml"))
    
    if(dataset=="game") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/game.xml"))

    if(dataset=="game_events") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/game_events.xml"))
    
    if(dataset=="inning_all") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_all.xml"))
    
    if(dataset=="inning_hit") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_hit.xml"))
    
    if(dataset=="linescore") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/linescore.xml"))
    
    if(tolower(dataset)=="openwar") glist <- url_gids

    return(glist)
}
