urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_09/day_12/gid_2017_09_12_chamlb_kcamlb_1/linescore.xml"
file <- read_xml(urlz)


# Make some place-holders for the function.
game <- list(); game_media <- list()
lnames <- list(game=game, game_media=game_media)
message("Gathering Gameday data, please be patient...")
out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=FALSE,
                        .final = function(x) stats::setNames(x, names(lnames)),
                        .init=list(list(), list(), list(), list(), list())) %dopar% {
                            file <- tryCatch(xml2::read_xml(urlz[[i]]), error=function(e) NULL)
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







    
    
    
    

for(i in seq_along(urlz)){
    out=urlz[[i]]
}


urlz=make_gids(start = "2015-09-03", end = "2015-09-10", cluster = NULL)

start=Sys.time() ; print(start)

# Make some place-holders for the function.
atbat_df <- list(); action_df <- list(); pitch_df <- list(); runner_df <- list(); po_df <- list()
lnames <- list(atbat=atbat_df, action=action_df, pitch=pitch_df, runner=runner_df, po=po_df)
for(i in seq_along(urlz)){
    file <- tryCatch(xml2::read_xml(urlz[[i]]), error=function(e) NULL)
    if(!is.null(file)){
        atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), xml2::xml_find_all(file, "./inning/bottom/atbat")) 
        action_nodes <- c(xml2::xml_find_all(file, "./inning/top/action"), xml2::xml_find_all(file, "./inning/bottom/actioin")) 
        pitch_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/pitch"),xml2::xml_find_all(file, "./inning/bottom/atbat/pitch"))
        runner_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/runner"), xml2::xml_find_all(file, "./inning/bottom/atbat/runner")) 
        po_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/po"), xml2::xml_find_all(file, "./inning/bottom/atbat/po"))
        url <- urlz[[i]]
        date_dt <- stringr::str_sub(urlz[[i]], 71, -39)
        gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
        
        atbat_df[[i]] <- purrr::map_dfr(atbat_nodes, function(x) {
            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
            out$url <- url
            out$date <- date_dt
            out$gameday_link <- gameday_link
            out
        })
        
        action_df[[i]] <- purrr::map_dfr(action_nodes, function(x) {
            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
            out$url <- url
            out$gameday_link <- gameday_link
            out
        })
        
        pitch_df[[i]] <- purrr::map_dfr(pitch_nodes, function(x) {
            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
            out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
            out$next_ <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next")
            out$inning_side <- xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))
            out$url <- url
            out$gameday_link <- gameday_link
            out
        })
        
        runner_df[[i]] <- purrr::map_dfr(runner_nodes, function(x) {
            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
            out$url <- url
            out$gameday_link <- gameday_link
            out
        })
        
        po_df[[i]] <- purrr::map_dfr(po_nodes, function(x) {
            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
            out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
            out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
            out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
            out$url <- url
            out$gameday_link <- gameday_link
            out
        })

    }
}

atbat <- dplyr::bind_rows(atbat_df)
action <- dplyr::bind_rows(action_df)
pitch <- dplyr::bind_rows(pitch_df)
runner <- dplyr::bind_rows(runner_df)
po <- dplyr::bind_rows(po_df)
innings_df <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)

end=Sys.time()
runtime = end - start
runtime
                        
