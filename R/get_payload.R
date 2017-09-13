
#' Get Gameday data from MLBAM.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param league The leauge to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "players",
#' @param cluster A named parallel cluster produced by the \code{doParallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' df <- get_payload(url)
#' }
#' 
get_payload <- function(start=NULL, end=NULL, league="mlb", dataset = "inning_all", cluster=NULL, ...) {
    start <- as.Date(as.character(start)); end <- as.Date(end); league <- tolower(league)
    if(start < as.Date("2008-01-01")){
        stop("Please select a later start date. The data are not dependable prior to 2008.")
    }
    if(end >= Sys.Date()) stop("Please select an earlier end date.")
    
    if(start > end) stop("Your start date appears to occur after your end date.")
    # Get gids via internal function.
    urlz <- make_gids(start = start, end = end, cluster = cluster)
    
    # Make some place-holders for the function.
    atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
    lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb", .multicombine=T, .inorder=FALSE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list(), list(), list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]]), error=function(e) NULL)
                                if(!is.null(file)){
                                    atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                                                     xml2::xml_find_all(file, "./inning/bottom/atbat")) 
                                    action_nodes <- c(xml2::xml_find_all(file, "./inning/top/action"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/actioin")) 
                                    pitch_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/pitch"),
                                                     xml2::xml_find_all(file, "./inning/bottom/atbat/pitch"))
                                    runner_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/runner"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/atbat/runner")) 
                                    po_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/po"), 
                                                  xml2::xml_find_all(file, "./inning/bottom/atbat/po"))
                                    
                                    list(                        
                                        atbat <- purrr::map_dfr(atbat_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                            out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
                                            out
                                        }),
                                        
                                        action <- purrr::map_dfr(action_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                                out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                                out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                                out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
                                                out
                                        }),
                                        
                                        pitch <- purrr::map_dfr(pitch_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next")
                                            out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))=="top", "top", "bottom")
                                            out
                                        }),
                                        
                                        runner <- purrr::map_dfr(runner_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                                out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                                out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                                out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
                                                out
                                        }),
                                        
                                        po <- purrr::map_dfr(po_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                                out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                                out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                                out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
                                                out
                                        })
                                    )
                                }
                            }

    # The foreach loop returns a named list of nested data frames. We need to bind the dfs under 
    # each name and pack the binded dfs back into a list that can be returned.
    atbat <- dplyr::bind_rows(out$atbat)
    action <- dplyr::bind_rows(out$action)
    pitch <- dplyr::bind_rows(out$pitch)
    runner <- dplyr::bind_rows(out$runner)
    po <- dplyr::bind_rows(out$po)
    innings_df <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    return(innings_df)
}

# I tried map and purrr::map as a non-cluster alternative here. This version is a bit slower than the
# foreach loop with no cluster. Slower due to the fact it's a nested loop. Need to revisit this.

#out <- map(names(inning), function(x){
#    out1 <- map(url, function(i) {
#        payload(i, node=as.character(x))
#    })
#})




