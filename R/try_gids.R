
#' Internal function to scrape gids from miniscorboard. The function returns a possibly valid gameday link.
#' @param gids2check A list of gid dates in format \code{year_2017/month_10/day_01} as used in the \code{make_gids()} function.
#' @param cluster name of the cluster if one exists.
#' @param ... Additional arguments.
#' @import foreach
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @keywords internal
#' @export
#' 
try_gids <- function(gids2check=NULL, cluster=NULL, ...) {
    root <- "http://gd2.mlb.com/components/game/"
    # Set class to "mini" so we can just use the payload function.
    try_urls <- gids2check %>% purrr::map(~ paste0(root, ., "/miniscoreboard.xml")) %>% 
        purrr::map(~ structure(., class = "mini"))
    
    # Grab game ids from miniscoreboard. Use foreach if there's a cluster, else use map().
    games=NULL
    ifelse(!is.null(cluster),
           games <- foreach::foreach(i = 1:length(try_urls)) %dopar% {
               payload(try_urls[[i]])
               
           },
           games <- purrr::map(try_urls, function(i){
               payload(i)
           }))
    
    # Make a list of proper gids.
    gidz <- dplyr::bind_rows(games) %>% select("gameday_link") %>% as.list()
}





