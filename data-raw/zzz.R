urlz <- "http://gd2.mlb.com/components/game/mlb/year_2016/month_09/day_29/gid_2016_09_29_bosmlb_nyamlb_1/inning/inning_all.xml"
file <- read_xml(urlz)

#pitch_nodes <- xml2::xml_find_all(file, "//inning/top/atbat/pitch")
#atbat_nodes <- xml2::xml_find_all(file, "//inning/top/atbat")
#action_nodes <- xml2::xml_find_all(file, "//inning/top/action")
#runner_nodes <- xml2::xml_find_all(file, "//inning/top/runner")
#po_nodes <- xml2::xml_find_all(file, "//inning/top/po")
nodes <- c(xml2::xml_find_all(file, "./inning/top"), 
           xml2::xml_find_all(file, "./inning/bottom")) 

atbat <- purrr::map_dfr(atbat_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
})

action <- purrr::map_dfr(action_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
})

pitch <- purrr::map_dfr(pitch_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))=="top", "top", "bottom")
    out
})

runner <- purrr::map_dfr(runner_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
})

po <- purrr::map_dfr(po_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
})