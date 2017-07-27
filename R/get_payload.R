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
#' @method payload player
#' @export

payload.player <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//team")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./player"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload inning_all
#' @export

payload.inning_all <- function(obj, ...) {
    file <- try(read_xml(obj[1]))
    top <- xml_find_all(file, "//top")
    bot <- xml_find_all(file, "//bottom")
    top <- bind_rows(lapply(top, function(x) {
        node_dat <- try(xml_find_all(x, "./atbat"), silent=FALSE)
        #if (inherits(node_dat, "try-error") | length(node_dat) == 0) print(warning("URL not found."))
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bot <- bind_rows(lapply(bot, function(x) {
        node_dat <- try(xml_find_all(x, "./atbat"), silent=FALSE)
        #if (inherits(node_dat, "try-error") | length(node_dat) == 0) print(warning("URL not found."))
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    dat <- bind_rows(top, bot)
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload inning_hit
#' @export

payload.inning_hit <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//hitchart")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./hip"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload mini
#' @export

payload.mini <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//games")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./game"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload game_events
#' @export

payload.game_events <- function(obj, ...) {
    file <- read_xml(obj[1])
    top <- xml_find_all(file, "//top")
    bot <- xml_find_all(file, "//bottom")
    top <- bind_rows(lapply(top, function(x) {
        pitches <- try(xml_find_all(x, "./atbat"), silent=FALSE)
        if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
        pitch_dat <- bind_rows(lapply(pitches, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    bot <- bind_rows(lapply(bot, function(x) {
        pitches <- try(xml_find_all(x, "./atbat"), silent=FALSE)
        if (inherits(pitches, "try-error") | length(pitches) == 0) return(NULL)
        pitch_dat <- bind_rows(lapply(pitches, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
    dat <- bind_rows(top, bot)
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload game
#' @export

payload.game <- function(obj, ...) {
    file <- read_xml(obj[1])
    xmldat <- xml_find_all(file, "//game")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./team"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
    }))
}

#' Get Gameday data from MLBAM.
#' @param url currently a url
#' @param ... additional arguments
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#' df <- get_payload(url)
#' }
#' 
get_payload <- function(url, ...) {
    # Workflow: Need to have a list of URLs. Loop through the list and assign a class to all of them.
    # Maybe assign url classes in a seperate makeURLs function rather than here??
    # Loop through URLs, pull data with correct OO function and rebind those into a single data frame.
    # Will probably have to do several loops, one for each group of URL classes.
    
    # validate the URLs, make sure none throw a 404 on innings_all becuase the game was cancelled.
    gamez=validurlz=NULL
    for(i in 1:length(url)){
        if(isTRUE(validURL(url[i]))){
            validurlz[i] <- url[i]
        }
    }
    
    for(i in 1:length(validurlz)){
        urlType <- stringr::str_extract(validurlz[i], '\\b[^/]+$')
        if(urlType=="inning_all.xml"){
            obj <- structure(validurlz[i], class = "inning_all")
            print(obj)
            dat <- payload(obj)
            gamez <- dplyr::bind_rows(gamez, dat)
        }
    }
    
    
    
}
    