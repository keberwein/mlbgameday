urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_09/day_12/miniscoreboard.xml"
file <- read_xml(urlz)

mini_nodes <- xml2::xml_find_all(file, "./game")


mini <- purrr::map_dfr(mini_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out
})