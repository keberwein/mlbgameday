



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


library(xml2);library(dplyr);library(magrittr)
url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/inning/inning_all.xml"
file <- read_xml(url)


atbat <- xml_find_all(file, "//top")
zpitch <- bind_rows(lapply(atbat, function(x) {
    pitches <- try(xml_find_all(x, "./atbat"), silent=FALSE)
    if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
    pitch_dat <- bind_rows(lapply(pitches, function(y) {
        data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
    }))
    atbat_num <- try(xml_attr(x, "num"))
    if (inherits(atbat_num, "try-error") | length(atbat_num) == 0) return(NULL)
    pitch_dat$atbat_num <- atbat_num
    pitch_dat
})) 


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

