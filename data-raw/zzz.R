urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_11/gid_2017_05_11_minmlb_chamlb_1/inning/inning_all.xml"
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


# If database connection is TRUE, read/write data in chunks of 1000, doing a gc() after every loop.
# We can put this in an outter loop in get_payload. It should slow things down, since it will only loop over a couple of times.
# If no db connection and data are bigger than 3000 urls, flip a warning and a user acknowlagement "Press Y to continue."

len_urlz <- length(urlz)

