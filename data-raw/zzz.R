urlz <- "http://gd2.mlb.com/components/game/mlb/year_2016/month_09/day_29/gid_2016_09_29_bosmlb_nyamlb_1/inning/inning_all.xml"
file <- read_xml(urlz)

inning <- xml2::xml_find_all(file, "//inning") 

pitch_nodes <- xml2::xml_find_all(file, "//inning/top/atbat/pitch")
atbat_nodes <- xml2::xml_find_all(file, "//inning/top/atbat")
#action_nodes <- xml2::xml_find_all(file, "//inning/top/action")
#runner_nodes <- xml2::xml_find_all(file, "//inning/top/runner")
#po_nodes <- xml2::xml_find_all(file, "//inning/top/po")
nodes <- xml2::xml_find_all(file, "//inning/top")


action <- dplyr::bind_rows(purrr::map(action_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE) %>% 
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
}))

runner <- dplyr::bind_rows(purrr::map(runner_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
}))

po <- dplyr::bind_rows(purrr::map(po_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE) 
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out
}))




