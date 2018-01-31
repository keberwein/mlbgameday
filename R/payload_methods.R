# These methods are more of a functional naming convention than OO programming. This was done so the get_payload script
# would be shorter and less crowded.

#' An internal function for bis_boxscore payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom dplyr bind_rows left_join rename mutate
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export
payload.gd_bis_boxscore <- function(urlz, ...) {
    batting <- list(); pitching <- list()
    lnames <- list(batting=batting,pitching=pitching)
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                                if(!isTRUE(is.null(file))){
                                    date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                                        as.Date(format = "%Y-%m-%d")
                                    gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                                    pitch_nodes <- xml2::xml_find_all(file, "/boxscore/pitching/pitcher")
                                    bat_nodes <- xml2::xml_find_all(file, "/boxscore/batting/batter")
                                    
                                    list(
                                        batting <- purrr::map_dfr(bat_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        pitching <- purrr::map_dfr(pitch_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        })
                                    )
                                }
                            }
    batting <- dplyr::bind_rows(out$batting)
    pitching <- dplyr::bind_rows(out$pitching)
    innings_df <- list(batting=batting, pitching=pitching)
    innings_df <- structure(innings_df, class="list_bis_boxscore") %>%
        transform_pload()
    return(innings_df)
    
}

#' An internal function for game_events paylaod.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export
#' 
payload.gd_game_events <- function(urlz, ...) {
    innings_df <- foreach::foreach(i = seq_along(urlz), .combine="rbind", .multicombine=T, .inorder=TRUE) %dopar% {
        file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
        if(!isTRUE(is.null(file))){
            pitch_nodes <- c(xml2::xml_find_all(file, "/game/inning/top/atbat/pitch"), 
                             xml2::xml_find_all(file, "/game/inning/bottom/atbat/pitch")) 
            
            date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                as.Date(format = "%Y-%m-%d")            
            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
            
            events <- purrr::map_dfr(pitch_nodes, function(x) {
                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                # An inner-loop would be more elegant here, but this way is faster, so...
                out$num <- xml2::xml_parent(x) %>% xml2::xml_attr("num")
                out$b <- xml2::xml_parent(x) %>% xml2::xml_attr("b")
                out$s <- xml2::xml_parent(x) %>% xml2::xml_attr("s")
                out$o <- xml2::xml_parent(x) %>% xml2::xml_attr("o")
                out$start_tfs <- xml2::xml_parent(x) %>% xml2::xml_attr("start_tfs")
                out$start_tfs_zulu <- xml2::xml_parent(x) %>% xml2::xml_attr("start_tfs_zulu")
                out$batter <- xml2::xml_parent(x) %>% xml2::xml_attr("batter")
                out$pitcher <- xml2::xml_parent(x) %>% xml2::xml_attr("pitcher")
                out$des <- xml2::xml_parent(x) %>% xml2::xml_attr("des")
                out$des_es <- xml2::xml_parent(x) %>% xml2::xml_attr("des_es")
                out$event_num <- xml2::xml_parent(x) %>% xml2::xml_attr("event_num")
                out$event <- xml2::xml_parent(x) %>% xml2::xml_attr("event")
                out$event_es <- xml2::xml_parent(x) %>% xml2::xml_attr("event_es")
                out$play_guid <- xml2::xml_parent(x) %>% xml2::xml_attr("play_guid")
                out$home_team_runs <- xml2::xml_parent(x) %>% xml2::xml_attr("home_team_runs")
                out$away_team_runs <- xml2::xml_parent(x) %>% xml2::xml_attr("away_team_runs")
                out$b1 <- xml2::xml_parent(x) %>% xml2::xml_attr("b1")
                out$b2 <- xml2::xml_parent(x) %>% xml2::xml_attr("b2")
                out$b3 <- xml2::xml_parent(x) %>% xml2::xml_attr("b3")
                out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
                out$inning_side <- xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))
                out$date <- date_dt
                out$gameday_link <- gameday_link
                out
            })
        }
    }
    innings_df <- structure(innings_df, class="df_game_events") %>%
        transform_pload()
    
    return(innings_df)
}



#' An internal function for inning_all payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export

payload.gd_inning_all <- function(urlz, ...) {
    # Make some place-holders for the function.
    atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
    lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list(), list(), list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                                if(!isTRUE(is.null(file))){
                                    atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                                                     xml2::xml_find_all(file, "./inning/bottom/atbat")) 
                                    action_nodes <- c(xml2::xml_find_all(file, "./inning/top/action"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/action"))
                                    # Make action nodes a child of atbat so we can get the at-bat number for which the action took place.
                                    for (a in seq_along(action_nodes)) {
                                        xml2::xml_add_child(atbat_nodes[[a]], action_nodes[[a]], .where = "after", free = T)
                                    }
                                    action_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/action"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/atbat/actioin"))
                                    
                                    pitch_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/pitch"),
                                                     xml2::xml_find_all(file, "./inning/bottom/atbat/pitch"))
                                    runner_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/runner"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/atbat/runner")) 
                                    po_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/po"), 
                                                  xml2::xml_find_all(file, "./inning/bottom/atbat/po"))
                                    url <- urlz[[i]]
                                    
                                    
                                    date_dt <- stringr::str_sub(urlz[[i]], 70, 81) %>% stringr::str_replace_all("_", "-") %>%
                                        as.Date(format = "%Y-%m-%d")
                                    gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                                    
                                    
                                    
                                    list(                        
                                        atbat <- purrr::map_dfr(atbat_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
                                            out$url <- url
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        action <- purrr::map_dfr(action_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        pitch <- purrr::map_dfr(pitch_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                            out$count <- paste(xml2::xml_parent(x) %>% xml2::xml_attr("b"), 
                                                               xml2::xml_parent(x) %>% xml2::xml_attr("s"), sep="-")
                                            out
                                        }),
                                        
                                        runner <- purrr::map_dfr(runner_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$num <- as.character(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        po <- purrr::map_dfr(po_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric( xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <-  as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
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
    
    innings_df <- structure(innings_df, class="list_inning_all") %>%
        transform_pload()
    
    return(innings_df)
}

#' An internal function for inning_hit payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export
#' 
payload.gd_inning_hit <- function(urlz, ...) {
    innings_df <- foreach::foreach(i = seq_along(urlz), .combine="rbind", .multicombine=T, .inorder=TRUE) %dopar% {
        file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
        if(!isTRUE(is.null(file))){
            date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                as.Date(format = "%Y-%m-%d")
            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
            hip_nodes <- xml2::xml_find_all(file, "/hitchart/hip")
            game <- purrr::map_dfr(hip_nodes, function(x) {
                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                out$date <- date_dt
                out$gameday_link <- gameday_link
                out
            })
        }
    }
    innings_df <- structure(innings_df, class="df_inning_hit") %>%
        transform_pload()
    return(innings_df)
}

#' An internal function for linescore payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom dplyr bind_rows rename
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export

payload.gd_linescore <- function(urlz, ...) {
    game <- list(); game_media <- list()
    lnames <- list(game=game, game_media=game_media)
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                                if(!isTRUE(is.null(file))){
                                    date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                                        as.Date(format = "%Y-%m-%d")
                                    gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                                    game_nodes <- xml2::xml_find_all(file, "/game")
                                    media_nodes <- xml2::xml_find_all(file, "/game/game_media/media")
                                    
                                    list(
                                        game <- purrr::map_dfr(game_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
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
    innings_df <- structure(innings_df, class="list_linescore") %>%
        transform_pload()
    return(innings_df)
}


#' An internal function for game payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export
#' 
payload.gd_game <- function(urlz, ...) {
    innings_df <- foreach::foreach(i = seq_along(urlz), .combine="rbind", .multicombine=T, .inorder=TRUE) %dopar% {
        file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
        if(!is.null(file)){
            date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                as.Date(format = "%Y-%m-%d")
            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
            game_nodes <- xml2::xml_find_all(file, "/game/team")
            game <- purrr::map_dfr(game_nodes, function(x) {
                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                out$type <- xml2::xml_parent(x) %>% xml2::xml_attr("type")
                out$local_game_time <- xml2::xml_parent(x) %>% xml2::xml_attr("local_game_time")
                out$game_pk <- xml2::xml_parent(x) %>% xml2::xml_attr("game_pk")
                out$game_time_et <- xml2::xml_parent(x) %>% xml2::xml_attr("game_time_et")
                out$gameday_sw <- xml2::xml_parent(x) %>% xml2::xml_attr("gameday_sw")
                out$date <- date_dt
                out$gameday_link <- gameday_link
                out
            })
        }
    }
    innings_df <- structure(innings_df, class="df_game") %>%
        transform_pload()
    return(innings_df)
}



