urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_11/gid_2017_05_11_minmlb_chamlb_1/game.xml"
file <- read_xml(urlz)

atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                 xml2::xml_find_all(file, "./inning/bottom/atbat")) 

atbat <- purrr::map_dfr(atbat_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_integer("num"))
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
    out$url <- url
    out$date <- date_dt
    out$gameday_link <- gameday_link
    out
})


game_nodes <- xml2::xml_find_all(file, "/game/team")
game <- purrr::map_dfr(game_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$type <- xml2::xml_parent(x) %>% xml2::xml_attr("type")
    out$local_game_time <- xml2::xml_parent(x) %>% xml2::xml_attr("local_game_time")
    out$game_pk <- xml2::xml_parent(x) %>% xml2::xml_attr("game_pk")
    out$game_time_et <- xml2::xml_parent(x) %>% xml2::xml_attr("game_time_et")
    out$gameday_sw <- xml2::xml_parent(x) %>% xml2::xml_attr("gameday_sw")
    out
})





