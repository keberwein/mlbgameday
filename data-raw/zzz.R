file <- xml2::read_xml("http://gd2.mlb.com/components/game/mlb/year_2016/month_09/day_29/gid_2016_09_29_bosmlb_nyamlb_1/inning/inning_all.xml")
inning <- xml2::xml_find_all(file, "//inning")

y = xml2::xml_find_all(inning, "./top/atbat/pitch")

y = xml2::xml_find_all(inning, "./top/action")

x = inning

node_dat <- c(xml2::xml_find_all(inning, "./top/atbat/pitch"), xml2::xml_find_all(inning, "./top/atbat"))

### Everything works except pitch, which is the biggest table. Possibly due to pitches with no inning???
### Try a couple normal loops to see if that speeds things up. Running about same as pitchRx in parallel

df <- dplyr::bind_rows(purrr::map(inning, function(x) {
    node_dat <- c(xml2::xml_find_all(inning, "./top/atbat/pitch"), xml2::xml_find_all(inning, "./bottom/atbat/pitch"))
    sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
        data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
    }))
}))

df <- dplyr::bind_rows(purrr::map(inning, function(x) {
    node_dat <- c(xml2::xml_find_all(x, "./top/atbat/pitch"), xml2::xml_find_all(x, "./bottom/atbat/pitch"))
    sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
        data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE) %>% 
            dplyr::mutate(inning = xml2::xml_attr(x, "num"), next_ = xml2::xml_attr(x, "next"),
                         inning_side = xml_name(xml_parent(xml_parent(y))))
    }))
}))


if(node=="action"){
    # action table
    df <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- c(xml2::xml_find_all(x, "./top/action"), xml2::xml_find_all(x, "./bottom/action"))
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE) %>% 
                dplyr::mutate(inning = xml2::xml_attr(x, "num"), next_ = xml2::xml_attr(x, "next"),
                              inning_side = xml2::xml_name(xml2::xml_parent(y)))
        }))
    }))
}

if(node=="pitch"){
    # action table
    tpit <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- xml2::xml_find_all(x, "./top/atbat/pitch")
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bpit <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- xml_find_all(x, "./bottom/atbat/pitch")
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    df <- dplyr::bind_rows(tpit, bpit)
}

if(node=="runner"){
    # action table
    trun <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- xml2::xml_find_all(x, "./top/atbat/runner")
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    brun <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- xml_find_all(x, "./bottom/atbat/runner")
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    df <- dplyr::bind_rows(trun, brun)
}

if(node=="po"){
    # action table
    tpo <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- xml2::xml_find_all(x, "./top/atbat/po")
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bpo <- dplyr::bind_rows(purrr::map(inning, function(x) {
        node_dat <- xml_find_all(x, "./bottom/atbat/po")
        sub_dat <- dplyr::bind_rows(purrr::map(node_dat, function(y) {
            data.frame(t(xml2::xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    df <- dplyr::bind_rows(tpo, bpo)
}








