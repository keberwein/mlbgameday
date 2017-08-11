#' Return data from a payload object.
#' @param obj An object created from a obj link
#' @param ... additional arguments
#' @export
#' @examples
#' \dontrun{
#' df <- payload(obj)}
#' 

payload <- function(obj, ...) UseMethod("payload", obj)

#' @rdname payload
#' @method payload default
#' @export
payload.default <- function(obj, ...) {
    warning("Please specify a valid MLBAM URL.")
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @method payload player
#' @export

payload.player <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//team")
    dat <- bind_rows(map(xmldat, function(x) {
        node_dat <- xml_find_all(x, "./player")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @method payload inning_all
#' @export

payload.inning_all <- function(obj, ...) {
    file <- read_xml(obj[1])
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
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @method payload inning_hit
#' @export

payload.inning_hit <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//hitchart")
    dat <- bind_rows(map(xmldat, function(x) {
        node_dat <- xml_find_all(x, "./hip")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @method payload mini
#' @export

payload.mini <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//games")
    dat <- bind_rows(map(xmldat, function(x) {
        node_dat <- xml_find_all(x, "./game")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @method payload game_events
#' @export

payload.game_events <- function(obj, ...) {
    file <- read_xml(obj[1])
    top <- xml_find_all(file, "//top")
    bot <- xml_find_all(file, "//bottom")
    top <- bind_rows(map(top, function(x) {
        pitches <- xml_find_all(x, "./atbat")
        pitch_dat <- bind_rows(map(pitches, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bot <- bind_rows(map(bot, function(x) {
        pitches <- xml_find_all(x, "./atbat")
        pitch_dat <- bind_rows(map(pitches, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    dat <- bind_rows(top, bot)
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @method payload game
#' @export

payload.game <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//game")
    dat <- bind_rows(map(xmldat, function(x) {
        node_dat <- xml_find_all(x, "./team")
        sub_dat <- bind_rows(map(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' Get Gameday data from MLBAM.
#' @param url currently a url.
#' @param cluster A named parallel cluster produced by the \code{parallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#' df <- get_payload(url)
#' }
#' 
get_payload <- function(url, cluster=NULL, ...) {
    inning_all=inning_hit=NULL
    # Workflow: Need to have a list of URLs. Loop through the list and assign a class to all of them.
    # Maybe assign url classes in a seperate makeURLs function rather than here??
    # Loop through URLs, pull data with correct OO function and rebind those into a single data frame.
    # Will probably have to do several loops, one for each group of URL classes.
    # 
    urlType=NULL
    # If we find a cluster, use a foreach loop, else use map loop.
    if(!is.null(cluster)){
        foreach::foreach(i = 1:length(url)) %dopar% {
            # Find URL types in gids list and apply correct method to each type.
            urlType <- stringr::str_extract(url[i], '\\b[^/]+$')
            if(urlType=="inning_hit.xml"){
                obj <- structure(url[i], class = "inning_hit")
                dat <- payload(obj)
                inning_hit <- dplyr::bind_rows(inning_hit, dat)
            }
            if(urlType=="inning_all.xml"){
                obj <- structure(url[i], class = "inning_all")
                dat <- payload(obj)
                inning_all <- dplyr::bind_rows(inning_all, dat)
            }
        }
    } else {
        for(i in 1:length(url)){
            # Find URL types in gids list and apply correct method to each type.
            urlType <- stringr::str_extract(url[i], '\\b[^/]+$')
            if(urlType=="inning_hit.xml"){
                obj <- structure(url[i], class = "inning_hit")
                dat <- payload(obj)
                inning_hit <- dplyr::bind_rows(inning_hit, dat)
            }
            if(urlType=="inning_all.xml"){
                obj <- structure(url[i], class = "inning_all")
                dat <- payload(obj)
                inning_all <- dplyr::bind_rows(inning_all, dat)
            }
        }
        return(inning_all)
    }
    #return(inning_all)
}
    
        
        


