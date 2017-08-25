#' Internal function to validate gids produced by try_gids.
#' @param gidslist a list of gids.
#' @param cluster name of the cluster if one exists.
#' @param league The leauge to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}.
#' @param ... additional arguments
#' @import foreach
#' @importFrom purrr map map_chr
#' @importFrom stringr str_sub str_length
#' @importFrom dplyr setdiff rename
#' @import foreach
#' @keywords internal
#' @export


validate_gids <- function(gidslist=NULL, league="mlb", cluster=NULL, ...) {
    # Rename leauge because it conflicts with some downloaded xml values.
    lg <- league
    # Create an emply list to hold the results of the loop.
    gidslist_dt=i=str_length=NULL
    root <- "http://gd2.mlb.com/components/game/"
    
    # Get start and end dates from gids.
    gidslist_dt <- as.data.frame(gidslist)
    gidslist_dt <- rename(gidslist_dt, gid = gidslist)
    gidslist_dt$link <- paste0(root, league, "/" ,gidslist_dt$gid)
    
    # Use miniscorboard to validate urls. Set class to mini so we can use the payload function.
    minilist <- gidslist_dt$link %>% purrr::map_chr(~ paste0(., "/miniscoreboard.xml")) %>% 
        purrr::map(~ structure(., class = "mini"))
    
    # Do a tryCatch along the dates. If the url exists, get the payload from miniscorboard.
    games=NULL
    ifelse(!is.null(cluster),
           games <- foreach::foreach(i = 1:length(minilist)) %dopar% {
                tryCatch(payload(minilist[[i]]), error=function(e) NULL)
           },
           games <- purrr::map(minilist, function(i){
                tryCatch(payload(i), error=function(e) NULL)
           }))
    
    gidz <- dplyr::bind_rows(games) %>%
        dplyr::mutate(url = paste0(root, lg, "/", "year_", str_sub(gameday_link, 1, 4), "/", "month_",
                                                str_sub(gameday_link, 6, 7), "/", "day_", str_sub(gameday_link, 9, 10), 
                                                "/gid_", gameday_link)) %>% 
        select(url)
    # Needs to be a list so payload will read it correct
    gidz <- gidz$url %>% as.list
    
    return(gidz)
}
