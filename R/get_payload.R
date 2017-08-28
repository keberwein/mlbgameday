#' Return data from a payload object.
#' @param obj An object created from a obj link
#' @param node The xml node to be scraped
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

payload.inning_all <- function(obj, node, ...) {
    # Grab all the xml nodes we need.
    file <- read_xml(obj[[1]])
    top <- xml_find_all(file, "//top")
    bot <- xml_find_all(file, "//bottom")
    ab <- xml_find_all(file, "//atbat")
    # Loop over nodes. There need to be different loops for top and bottom of inning, due to the xml nesting.
    # Get atbat table.
    if(node=="atbat"){
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
        df <- bind_rows(top, bot)
    }
    
    if(node=="action"){
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
        df <- dplyr::bind_rows(tact, bact)
    }
    if(node=="pitch"){
        # pitch table
        df <- bind_rows(map(ab, function(x) {
            node_dat <- xml_find_all(x, "./pitch")
            sub_dat <- bind_rows(map(node_dat, function(y) {
                data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
            }))
        }))
    }
    if(node=="runner"){
        # runner table
        df <- bind_rows(map(ab, function(x) {
            node_dat <- xml_find_all(x, "./runner")
            sub_dat <- bind_rows(map(node_dat, function(y) {
                data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
            }))
        })) 
    }
    if(node=="po"){
        # po table
        df <- bind_rows(map(ab, function(x) {
            node_dat <- xml_find_all(x, "./po")
            sub_dat <- bind_rows(map(node_dat, function(y) {
                data.frame(t(xml_attrs(y)), stringsAsFactors=FALSE)
            }))
        }))
    }
    return(df)
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
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param league The leauge to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "players",
#' @param cluster A named parallel cluster produced by the \code{doParallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' df <- get_payload(url)
#' }
#' 
get_payload <- function(start=NULL, end=NULL, league="mlb", dataset = "inning_all", cluster=NULL, ...) {
    start <- as.Date(as.character(start)); end <- as.Date(end); league <- tolower(league)
    if(start < as.Date("2008-01-01")){
        stop("Please select a later start date. The data are not dependable prior to 2008.")
    }
    if(end >= Sys.Date()) stop("Please select an earlier end date.")
    
    if(start > end) stop("Your start date appears to occur after your end date.")
    
    urlz <- make_gids(start = start, end = end, cluster = cluster)
    
    atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
    lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    
    out <- foreach::foreach(i = length(urlz), .combine="comb", .multicombine=T, .inorder=FALSE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list(), list(), list(), list())) %dopar% {
                                list(                        
                                    atbat <- tryCatch(payload(urlz[[i]], node="atbat"), error=function(e) NULL),
                                    action <- tryCatch(payload(urlz[[i]], node="action"), error=function(e) NULL),
                                    pitch <- tryCatch(payload(urlz[[i]], node="pitch"), error=function(e) NULL),
                                    runner <- tryCatch(payload(urlz[[i]], node="runner"), error=function(e) NULL),
                                    po <- tryCatch(payload(urlz[[i]], node="po"), error=function(e) NULL))
                            }
    
    # The foreach loop returns a named list of nested data frames. We need to bind the dfs under 
    # each name and pack the binded dfs back into a list that can be returned.
    atbat <- dplyr::bind_rows(out$atbat)
    action <- dplyr::bind_rows(out$action)
    pitch <- dplyr::bind_rows(out$pitch)
    runner <- dplyr::bind_rows(out$runner)
    po <- dplyr::bind_rows(out$po)
    
    innings_df <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    
    # Release uneeded objects from memory and return innings_df.
    rm(atbat, action, pitch, runner, po)
    gc() # May not need this here. Test to see if R returns memory without gc().
    return(innings_df)
}

# I tried map and lapply as a non-cluster alternative here. This version is a bit slower than the
# foreach loop with no cluster. Slower due to the fact it's a nested loop. Need to revisit this.

#out <- map(names(inning), function(x){
#    out1 <- map(url, function(i) {
#        payload(i, node=as.character(x))
#    })
#})




