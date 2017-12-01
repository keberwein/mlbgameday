#' Internal function to validate gids produced by try_gids.
#' @param gidslist a list of gids.
#' @param league The league to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}.
#' @param ... additional arguments
#' @import foreach
#' @importFrom purrr map map_chr
#' @importFrom stringr str_sub str_length
#' @importFrom dplyr setdiff rename select
#' @import foreach
#' @keywords internal
#' @export

validate_gids <- function(gidslist=NULL, league="mlb", ...) {
    # Rename league because it conflicts with some downloaded xml values.
    lg <- league
    # Create an emply list to hold the results of the loop.
    gidslist_dt=i=str_length=NULL
    root <- "http://gd2.mlb.com/components/game/"
    
    # Get start and end dates from gids.
    gidslist_dt <- as.data.frame(gidslist)
    gidslist_dt <- rename(gidslist_dt, gid = gidslist)
    gidslist_dt$link <- paste0(root, league, "/" ,gidslist_dt$gid)
    
    # Use miniscorboard to validate urls. Set class to mini so we can use the payload function.
    minilist <- gidslist_dt$link %>% purrr::map_chr(~ paste0(., "/miniscoreboard.xml"))
    
    # Do a tryCatch along the dates. If the url exists, get the payload from miniscorboard.
    out <- foreach::foreach(i = seq_along(minilist), .inorder=FALSE) %dopar% {
                                file <- tryCatch(xml2::read_xml(minilist[[i]]), error=function(e) NULL)
                                if(!is.null(file)){
                                    mini_nodes <- xml2::xml_find_all(file, "./game")
                                        mini <- purrr::map_dfr(mini_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out
                                        })
                                }
    }
    
    games <- dplyr::bind_rows(out)
    
    gidz <- games %>%
        dplyr::mutate(url = paste0(root, lg, "/", "year_", str_sub(gameday_link, 1, 4), "/", "month_",
                                                str_sub(gameday_link, 6, 7), "/", "day_", str_sub(gameday_link, 9, 10), 
                                                "/gid_", gameday_link)) %>% 
        select(url)
    # Needs to be a list so payload will read it correct
    gidz <- gidz$url %>% as.list
    
    return(gidz)
}
