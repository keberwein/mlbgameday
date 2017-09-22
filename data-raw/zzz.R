urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_11/gid_2017_05_11_minmlb_chamlb_1/game_events.xml"
file <- read_xml(urlz)






pitch_nodes <- c(xml2::xml_find_all(file, "/game/inning/top/atbat/pitch"), 
                  xml2::xml_find_all(file, "/game/inning/bottom/atbat/pitch")) 
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
    out$even <- xml2::xml_parent(x) %>% xml2::xml_attr("event")
    out$event_es <- xml2::xml_parent(x) %>% xml2::xml_attr("event_es")
    out$play_guid <- xml2::xml_parent(x) %>% xml2::xml_attr("play_guid")
    out$home_team_runs <- xml2::xml_parent(x) %>% xml2::xml_attr("home_team_runs")
    out$away_team_runs <- xml2::xml_parent(x) %>% xml2::xml_attr("away_team_runs")
    out$b1 <- xml2::xml_parent(x) %>% xml2::xml_attr("b1")
    out$b2 <- xml2::xml_parent(x) %>% xml2::xml_attr("b2")
    out$b3 <- xml2::xml_parent(x) %>% xml2::xml_attr("b3")
    out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
    out$inning_side <- xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))
    out
})


# Make some place-holders for the function.
batting <- list(); pitching <- list()
lnames <- list(batting=batting,pitching=pitching)
message("Gathering Gameday data, please be patient...")
out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=FALSE,
                        .final = function(x) stats::setNames(x, names(lnames)),
                        .init=list(list(), list())) %dopar% {
                            file <- tryCatch(xml2::read_xml(urlz[[i]]), error=function(e) NULL)
                            if(!is.null(file)){
                                pitch_nodes <- xml2::xml_find_all(file, "/boxscore/pitching/pitcher")
                                bat_nodes <- xml2::xml_find_all(file, "/boxscore/batting/batter")

                                list(
                                    batting <- purrr::map_dfr(bat_nodes, function(x) {
                                        out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                        out
                                    }),
                                    
                                    pitching <- purrr::map_dfr(pitch_nodes, function(x) {
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
                        
