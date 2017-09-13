urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_09/day_12/gid_2017_09_12_chamlb_kcamlb_1/inning/inning_all.xml"
file <- read_xml(urlz)

atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                 xml2::xml_find_all(file, "./inning/bottom/atbat")) 

atbat_nodes <- xml2::xml_find_all(file, "./inning/top/atbat")

atbat <- data.table::rbindlist(purrr::map(atbat_nodes, function(x) {
    out <- data.table::as.data.table(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$next_ <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next")
    out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(x))=="top", "top", "bottom")
    out}), fill = TRUE)

# Need to figure out how to cast the above to a data.table and try to loop through that.

for(x in seq_along(atbat_nodes)){
    out <- data.table::as.data.table(t(xml2::xml_attrs(atbat_nodes[[x]])), stringsAsFactors=FALSE)
    #out$inning <- xml2::xml_parent(xml2::xml_parent(atbat_nodes[[x]])) %>% xml2::xml_attr("num")
    #out$next_ <- xml2::xml_parent(xml2::xml_parent(atbat_nodes[[x]])) %>% xml2::xml_attr("next")
    #out$inning_side <- ifelse(xml2::xml_name(xml2::xml_parent(atbat_nodes[[x]]))=="top", "top", "bottom")
    #dt <- data.table::rbindlist(out[x])
}