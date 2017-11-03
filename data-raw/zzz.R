atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)

out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=FALSE,
                        .final = function(x) stats::setNames(x, names(lnames)),
                        .init=list(list(), list(), list(), list(), list())) %dopar% {
                            
                            start=Sys.time()
                            
                            file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                            
                            end=Sys.time()
                            runtime = end - start
                            print(paste0("trycatch ", runtime))
                            
                            if(!is.null(file)){
                                #message(as.character(urlz[[i]]))
                                
                                start=Sys.time()
                                
                                
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
                                date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                                    as.Date(format = "%Y-%m-%d")
                                gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                                
                                end=Sys.time()
                                runtime = end - start
                                print(paste0("nodes and links ", runtime))
                                
                                
                                start=Sys.time()
                                
                                
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
                                        out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                        out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                        out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
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
                                        out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                        out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                        out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
                                        out$url <- url
                                        out$gameday_link <- gameday_link
                                        out
                                    }),
                                    
                                    po <- purrr::map_dfr(po_nodes, function(x) {
                                        out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                        out$inning <- as.numeric( xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                        out$next_ <-  as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                        out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                        out$url <- url
                                        out$gameday_link <- gameday_link
                                        out
                                    })
                                )
                            }
                        }


end=Sys.time()
runtime = end - start
print(paste0("list ", runtime))


start=Sys.time()


# The foreach loop returns a named list of nested data frames. We need to bind the dfs under 
# each name and pack the binded dfs back into a list that can be returned.
atbat <- dplyr::bind_rows(out$atbat)
action <- dplyr::bind_rows(out$action)
pitch <- dplyr::bind_rows(out$pitch)
runner <- dplyr::bind_rows(out$runner)
po <- dplyr::bind_rows(out$po)

innings_df <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)

end=Sys.time()
runtime = end - start
print(paste0("bind rows ", runtime))

start=Sys.time()

# Add batter and pitcher names to the atbat data frame
player.env <- environment()
data(player_ids, package="mlbgameday", envir=player.env)
player_ids$id <- as.character(player_ids$id)

innings_df$atbat %<>% dplyr::left_join(player_ids, by = c("batter" = "id")) %>% 
    dplyr::left_join(player_ids, by = c("pitcher" = "id")) %>% 
    dplyr::rename(batter_name=full_name.x, pitcher_name=full_name.y)

end=Sys.time()
runtime = end - start
print(paste0("left join ", runtime))

innings_df <- structure(innings_df, class="list_inning_all") #%>%
#transform_pload()