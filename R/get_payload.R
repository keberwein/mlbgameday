# Update Gameday IDs.
#
# This function scrapes "gameday_links" from the MLB website.
# It should only be called when the user enters an end date later than the most recent ID
#
# @param last.date most recent date found in existing IDs
# @param end any date more recent than last.date
updateGids <- function(last.date, end) {
    message("grabbing new game IDs")
    # Get the gids
    scoreboards <- paste0(makeUrls(start=last.date, end=end, gids=""), "/miniscoreboard.xml")
    
    
    obs <- XML2Obs(scoreboards) #Note to future self -- using `xpath='//game[@gameday_link]'` for future games gives the RCurl error -- Recv failure: Connection reset by peer
    obs2 <- obs[grep("^games//game$", names(obs))]
    gids <- collapse_obs(obs2)[,"gameday_link"]
    paste0("gid_", gids[!is.na(gids)])
}


last.date="2017-06-01"
end = "2017-07-01"

makeUrls <- function(start, end, gids="infer") {
    root <- "http://gd2.mlb.com/components/game/mlb/"
    if (all(gids %in% "infer")) {
        if (missing(start) || missing(end)) {
            warning("Can't 'infer' game urls without start/end date.")
            return(root)
        } else {
            start <- as.POSIXct(start)
            end <- as.POSIXct(end)
            env <- environment()
            data(gids, package="pitchRx", envir=env)
            last.game <- strsplit(gids[length(gids)], split="_")[[1]]
            last.date <- as.POSIXct(paste(last.game[2], last.game[3], last.game[4], sep="-"))
            if (last.date < end) gids <- c(gids, updateGids(max(start, last.date), end))
            return(gids2urls(subsetGids(gids, first=start, last=end)))
        }
    } else {
        gidz <- gids[grep("gid", gids)]
        if (length(gidz) != length(gids)) {
            #message("The option gids was ignored since some values did not contain 'gid'")
            return(paste0(root, dates2urls(as.POSIXct(start), as.POSIXct(end))))
        } else {
            return(gids2urls(gidz))
        }
    }
}

# Take a game ID and construct url path for each specific game
gids2urls <- function(x) {
    root <- "http://gd2.mlb.com/components/game/"
    # Assume the league is 'mlb' unless we find evidence otherwise
    league <- rep("mlb", length(x))
    not.mlb <- !grepl("mlb", x)
    # If 'mlb' does not appear in the gid, use the home team's league
    if (any(not.mlb)) league[not.mlb] <- substr(x[not.mlb], 26, 28)
    base <- paste0(root, league)
    paste0(base, "/year_", substr(x, 5, 8), "/month_", substr(x, 10, 11),
           "/day_", substr(x, 13, 14), "/", x)
}

#Take a start and an end date and make vector of "year_XX/month_XX/day_XX"
dates2urls <- function(first.day, last.day) {
    dates <- seq(as.Date(first.day), as.Date(last.day), by = "day")
    paste0("year_", format(dates, "%Y"), "/month_",
           format(dates, "%m"), "/day_", format(dates, "%d"))
}


XML2Obs <- function(urls, xpath, append.value = TRUE, as.equiv = TRUE, url.map = FALSE, 
                    local = FALSE, quiet = FALSE, ...) {
    if (missing(xpath)) xpath <- "/"  #select the root
    docs <- urlsToDocs(urls, local, quiet, ...)
    valid.urls <- sapply(docs, function(x) attr(x, "XMLsource"))
    nodes <- docsToNodes(docs, xpath) 
    rm(docs)
    gc()
    l <- nodesToList(nodes)
    rm(nodes)
    gc()
    obs <- listsToObs(l, urls = valid.urls, append.value = append.value, 
                      as.equiv = as.equiv, url.map = url.map)
    # throw out url column if there is only one
    if (length(urls) == 1) obs <- lapply(obs, function(x) x[, !grepl("^url$", colnames(x)), drop = FALSE])
    obs
}

urlsToDocs <- function(urls, local = FALSE, quiet = FALSE, ...) {
    #keep only urls that exist
    if (!local) {
        urls <- urls[vapply(urls, url_ok, logical(1), USE.NAMES=FALSE)]
        text <- lapply(urls, function(x) content(GET(x, ...), as = "text"))
    } else {
        text <- urls
    }
    if (length(text) == 0) {
        warning("No content found. Please double check your urls.")
        return(text)
    }
    docs <- NULL
    for (i in seq_along(text)) {
        if (!quiet) cat(urls[i], "\n")
        doc <- try_default(xmlParse(text[i], asText = !local), NULL, quiet = TRUE)
        if (!is.null(doc)) {
            attr(doc, "XMLsource") <- urls[i]
            docs <- c(docs, doc) #Keep non-empty documents
        }
    }
    return(docs)
}


docsToNodes <- function(docs, xpath) {
    #I should really figure which class I want...
    rapply(docs, function(x) getNodeSet(x, path=xpath), 
           classes=c('XMLInternalDocument', 'XMLAbstractDocument'), how="replace")
}









##### work
# sample gid: http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/inning/inning_1.xml
# 
url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/players.xml"
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


library(xml2);library(dplyr);library(magrittr)
url <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_01/gid_2017_05_01_balmlb_bosmlb_1/inning/inning_all.xml"
file <- read_xml(url)
atbat <- xml_find_all(file, "//top")

pitch <- bind_rows(lapply(atbat, function(x) {
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
file <- read_xml(url)
atbat <- xml_find_all(file, "//games")

mini <- bind_rows(lapply(atbat, function(x) {
    pitches <- try(xml_find_all(x, "./game"), silent=FALSE)
    if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
    pitch_dat <- bind_rows(lapply(pitches, function(y) {
        data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
    }))
    atbat_num <- try(xml_attr(x, "num"))
    if (inherits(atbat_num, "try-error") | length(atbat_num) == 0) return(NULL)
    pitch_dat$atbat_num <- atbat_num
    pitch_dat
}))
