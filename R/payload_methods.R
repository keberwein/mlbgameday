# These methods are more of a functional naming convention than OO programming. This was done so the get_payload script
# would be shorter and less crowded. URLs are set as class objects by the game_urls function and these methods
# execute the appropriate function.

#' Return data from a payload object.
#' @param obj An object created from a obj link
#' @param ... additional arguments
#' @export
#' @examples
#' \dontrun{
#' df <- payload(obj)}
#' 

payload <- function(obj, ...) UseMethod("payload", obj)

#' @rdname payload
#' @method payload default
#' @export
payload.default <- function(obj, ...) {
    warning("Please specify a valid MLBAM URL.")
}


#' @rdname payload
#' @import xml2
#' @importFrom stringr str_sub
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method payload gd_inning_all
#' @export

payload.gd_inning_all <- function(obj, ...) {
    urlz <- obj
    # Make some place-holders for the function.
    atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
    lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    message("Gathering Gameday data, please be patient...")
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=FALSE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list(), list(), list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]]), error=function(e) NULL)
                                if(!is.null(file)){
                                    #message(as.character(urlz[[i]]))
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
                                    url <- urlz[[i]]
                                    date_dt <- stringr::str_sub(urlz[[i]], 71, -39)
                                    gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                                    
                                    list(                        
                                        atbat <- purrr::map_dfr(atbat_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
                                            out$url <- url
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        action <- purrr::map_dfr(action_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        pitch <- purrr::map_dfr(pitch_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next")
                                            out$inning_side <- xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out$num <- xml2::xml_parent(x) %>% xml2::xml_attr("num")
                                            out$count <- paste(xml2::xml_parent(x) %>% xml2::xml_attr("b"), 
                                                               xml2::xml_parent(x) %>% xml2::xml_attr("s"), sep="-")
                                            out
                                        }),
                                        
                                        runner <- purrr::map_dfr(runner_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        po <- purrr::map_dfr(po_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
                                            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
                                            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
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
    # Add batter and pitcher names to the atbat data frame
    player.env <- environment()
    data(player_ids, package="mlbgameday", envir=player.env)
    player_ids$id <- as.character(player_ids$id)
    
    innings_df$atbat %<>% dplyr::left_join(player_ids, by = c("batter" = "id")) %>% 
        dplyr::left_join(player_ids, by = c("pitcher" = "id")) %>% 
        dplyr::rename(batter_name=full_name.x, pitcher_name=full_name.y)
    
    return(innings_df)
    
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows rename
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method payload gd_linescore
#' @export

payload.gd_linescore <- function(obj, ...) {
    urlz <- obj
    game <- list(); game_media <- list()
    lnames <- list(game=game, game_media=game_media)
    message("Gathering Gameday data, please be patient...")
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=FALSE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]]), error=function(e) NULL)
                                if(!is.null(file)){
                                    game_nodes <- xml2::xml_find_all(file, "/game")
                                    media_nodes <- xml2::xml_find_all(file, "/game/game_media/media")
                                    
                                    list(
                                        game <- purrr::map_dfr(game_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out
                                        }),
                                        
                                        game_media <- purrr::map_dfr(media_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out
                                        })
                                    )
                                }
                            }
    game <- dplyr::bind_rows(out$game)
    game_media <- dplyr::bind_rows(out$game_media)
    innings_df <- list(game=game, game_media=game_media)
    return(innings_df)
}
