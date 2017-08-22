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
#' @param url currently a url.
#' @param cluster A named parallel cluster produced by the \code{doParallel} package.
#' @param ... additional arguments
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' df <- get_payload(url)
#' }
#' 
get_payload <- function(url, cluster=NULL, ...) {
    # Need to find way to return a list of data frames.
    # Re-write the bottom loop OR just have foreeach do both cases, will throw warning, but maybe ok.
    inning_all=inning_hit=hit_dat=all_dat=NULL
    #ifelse(!is.null(cluster),
    #       message("Cluster detected! Data collection may take a while, please be patient."),
    #       message("Data collection may take a while. If it takes more than 15 minutes, consider using parallel."))
    #       
    
#    mini_dat <- list()
#    mini <- foreach::foreach(i = 1:length(url), .combine = dplyr::bind_rows) %dopar% {
#        # Find URL types in gids list and apply correct method to each type.
#        if(isTRUE(class(url[[i]])=="mini")){
#            all_dat[[i]] <- payload(url[[i]])
#        }
#    }
    
#    player_dat <- list()
#    player <- foreach::foreach(i = 1:length(url), .combine = dplyr::bind_rows) %dopar% {
#        # Find URL types in gids list and apply correct method to each type.
#        if(isTRUE(class(url[[i]])=="player")){
#            all_dat[[i]] <- payload(url[[i]])
#        }
#    }
    
#    hit_dat <- list()
#    inning_hit <- foreach::foreach(i = 1:length(url), .combine = dplyr::bind_rows) %dopar% {
#        # Find URL types in gids list and apply correct method to each type.
#        if(isTRUE(class(url[[i]])=="inning_hit")){
#            hit_dat[[i]] <- payload(url[[i]])
#        }
#    }

    #atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
    lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    
    # Need to figure out how to make this one foreach loop. Five loops here seems like too much.
    out <- foreach::foreach(i = seq_along(url), .combine="comb", .multicombine=T,
                            .final = function(x) setNames(x, names(lnames)),
                            .init=list(list(), list(), list(), list(), list())) %dopar% {
                                list(                        
                                    atbat <- payload(url[[i]], node="atbat"),
                                    action <- payload(url[[i]], node="action"),
                                    pitch <- payload(url[[i]], node="pitch"),
                                    runner <- payload(url[[i]], node="runner"),
                                    po <- payload(url[[i]], node="po"))
                            }
    # The foreach loop returns a named list of nested data frames. We need to bind the dfs under 
    # each name and pack the binded dfs back into a list that can be returned.
    
    
}

#list2env(df[[1]] ,.GlobalEnv)

#z <- out$po

#z = out[[2]]
#list <- unlist(out$po, recursive = F)
#zz <- do.call("rbind", list)
z = bind_rows(out[[1]])
zz = bind_rows(out[[2]])

ab = bind_rows(out$atbat)



comb <- function(x, ...) {
    lapply(1:5, function(i) c(x[[i]], lapply(list(...), function(y) y[,i])))
}