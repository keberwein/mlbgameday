#' Return data from a payload object.
#' @param obj An object created from a obj link
#' @param ... currently ignored
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
    obj <- structure(obj, class = "character")
    file <- read_xml(obj)
    xmldat <- xml_find_all(file, "//team")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./player"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
        num_dat <- try(xml_attr(x, "num"))
        if (inherits(num_dat, "try-error") | length(num_dat) == 0) return(NULL)
        sub_dat$num_dat <- num_dat
        sub_dat
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload pitch
#' @export

payload.pitch <- function(obj, ...) {
    file <- read_xml(obj)
    xmldat <- xml_find_all(file, "//top")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./atbat"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
        num_dat <- try(xml_attr(x, "num"))
        if (inherits(num_dat, "try-error") | length(num_dat) == 0) return(NULL)
        sub_dat$num_dat <- num_dat
        sub_dat
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload inning
#' @export

payload.inning <- function(obj, ...) {
    file <- read_xml(obj)
    xmldat <- xml_find_all(file, "//hitchart")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./hip"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
        num_dat <- try(xml_attr(x, "num"))
        if (inherits(num_dat, "try-error") | length(num_dat) == 0) return(NULL)
        sub_dat$num_dat <- num_dat
        sub_dat
    }))
}

#' @rdname payload
#' @import xml2
#' @importFrom dplyr bind_rows
#' @method payload mini
#' @export

payload.mini <- function(obj, ...) {
    file <- read_xml(obj)
    xmldat <- xml_find_all(file, "//games")
    dat <- bind_rows(lapply(xmldat, function(x) {
        node_dat <- try(xml_find_all(x, "./game"), silent=FALSE)
        if (inherits(node_dat, "try-error") | length(node_dat) == 0) return(NULL)
        sub_dat <- bind_rows(lapply(node_dat, function(y) {
            data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
        }))
        num_dat <- try(xml_attr(x, "num"))
        if (inherits(num_dat, "try-error") | length(num_dat) == 0) return(NULL)
        sub_dat$num_dat <- num_dat
        sub_dat
    }))
}

#' Get Gameday data from MLBAM.
#' @param url currently a url
#' @param ... currently ignored
#' @importFrom stringr str_extract
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
    
    urlType <- stringr::str_extract(url, '\\b[^/]+$')
    
    ifelse(urlType=="inning_all.xml", {obj <- structure(url, class = "inning"); innings <- payload(obj)},
           ifelse())
    

    
    return(dat)
}
