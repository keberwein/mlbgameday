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
    
    # Use foreach if parallel, otherwise use purrr::map.
    #invalid=NULL
    #ifelse(!is.null(cluster),
    #       invalid <- foreach::foreach(i = 1:length(minilist)) %dopar% {
    #           if(!isTRUE(tidygameday::urlTrue(minilist[i]))){
    #               invalid[i] <- minilist[i]
    #           }
    #       },
    #       invalid <- purrr::map(minilist, function(i){
    #           if(!isTRUE(tidygameday::urlTrue(i))){
    #               output <- i
    #           }
    #       }))
    
    # Do a tryCatch along the dates. If the url exists, get the payload from miniscorboard.
    games=NULL
    ifelse(!is.null(cluster),
           games <- foreach::foreach(i = 1:length(minilist)) %dopar% {
               if(isTRUE(tidygameday::urlTrue(minilist[[i]]))){
                   payload(minilist[[i]])
               }
           },
           games <- purrr::map(minilist, function(i){
               if(isTRUE(tidygameday::urlTrue(i))){
                   minilist(i)
               }
               
           }))
    
    return(gidz)
}
