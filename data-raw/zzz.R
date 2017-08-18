



##### work
# sample gid: http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/inning/inning_1.xml
# 

library(dplyr);library(xml2)


url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/players.xml"

player <- function(){
    file <- read_xml(url)
    atbat <- xml_find_all(file, "//team")
    player <- bind_rows(lapply(atbat, function(x) {
        pitches <- try(xml_find_all(x, "./player"), silent=FALSE)
        if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
        pitch_dat <- bind_rows(lapply(pitches, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
        atbat_num <- try(xml_attr(x, "num"))
        if (inherits(atbat_num, "try-error") | length(atbat_num) == 0) return(NULL)
        pitch_dat$atbat_num <- atbat_num
        pitch_dat
    }))
}


library(xml2);library(dplyr);library(magrittr);library(purr)
obj <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/inning/inning_all.xml"
file <- read_xml(obj)


payload.inning_all <- function(obj, ...) {
    # Grab all the xml nodes we need.
    file <- read_xml(obj[1])
    top <- xml_find_all(file, "//top")
    bot <- xml_find_all(file, "//bottom")
    ab <- xml_find_all(file, "//atbat")
    
    # Loop over nodes. There need to be different loops for top and bottom of inning, due to the xml nesting.
    # Get atbat table.
    tab <- bind_rows(map(top, function(x) {
        node_dat <- xml_find_all(x, "./atbat")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bab <- bind_rows(map(bot, function(x) {
        node_dat <- xml_find_all(x, "./atbat")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    atbat <- bind_rows(tab, bab)
    # action table
    tact <- bind_rows(map(top, function(x) {
        node_dat <- xml_find_all(x, "./action")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bact <- bind_rows(map(bot, function(x) {
        node_dat <- xml_find_all(x, "./action")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    action <- dplyr::bind_rows(tact, bact)
    # pitch table
    pitch <- bind_rows(map(ab, function(x) {
        node_dat <- xml_find_all(x, "./pitch")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    # runner table
    runner <- bind_rows(map(ab, function(x) {
        node_dat <- xml_find_all(x, "./runner")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    # po table
    po <- bind_rows(map(ab, function(x) {
        node_dat <- xml_find_all(x, "./po")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    # Bind tables into a list of data frames.
    df <- list(atbat, action, pitch, runner, po)
}




url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/inning/inning_hit.xml"

file <- read_xml(url)
atbat <- xml_find_all(file, "//hitchart")

inning_hit <- bind_rows(lapply(atbat, function(x) {
    pitches <- try(xml_find_all(x, "./hip"), silent=FALSE)
    if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
    pitch_dat <- bind_rows(lapply(pitches, function(y) {
        data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
    }))
    atbat_num <- try(xml_attr(x, "num"))
    if (inherits(atbat_num, "try-error") | length(atbat_num) == 0) return(NULL)
    pitch_dat$atbat_num <- atbat_num
    pitch_dat
}))


url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/miniscoreboard.xml"

if(urlTrue(url)){print("yay")}

file <- read_xml(url)
atbat <- xml_find_all(file, "//games")

mini <- bind_rows(map(atbat, function(x) {
    pitches <- xml_find_all(x, "./game")
    #if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
    
    pitch_dat <- bind_rows(map(pitches, function(y) {
        data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
    }))
    
    atbat_num <- try(xml_attr(x, "num"))
    #if (inherits(atbat_num, "try-error") | length(atbat_num) == 0) return(NULL)
    pitch_dat$atbat_num <- atbat_num
    pitch_dat
}))



url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/game_events.xml"
file <- read_xml(url)
top <- xml_find_all(file, "//top")
bot <- xml_find_all(file, "//bottom")



    top <- bind_rows(map(top, function(x) {
        node_dat <- xml_find_all(x, "./atbat")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    
    
    bot <- bind_rows(map(bot, function(x) {
        node_dat <- xml_find_all(x, "./atbat")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    dat <- bind_rows(top, bot)
    
    
    

url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/game.xml"
file <- read_xml(url)
xmldat <- xml_find_all(file, "//game")
dat <- bind_rows(lapply(xmldat, function(x) {
    node_dat <- try(xml_find_all(x, "./team"), silent=FALSE)
    if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
    sub_dat <- bind_rows(lapply(node_dat, function(y) {
        data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
    }))
}))




purrr::map(url[[1]], function(i){
    if(isTRUE(class(i)=="inning_hit")){
        dat <- payload(i)
        inning_hit <- dplyr::bind_rows(inning_hit, dat)
    }
    if(isTRUE(class(i)=="inning_all")){
        dat <- payload(i)
        inning_all <- dplyr::bind_rows(inning_all, dat)
    }
    #df <- list(inning_hit, inning_all)
})

