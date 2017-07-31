#' Create urls from game_ids.
#' @param game_id A single \code{game_id} or a list of game_ids
#' @param ... additional arguments
#' @importFrom stringr str_sub
#' @export
#' @examples
#' \dontrun{
#' urls <- gameid2url(gids)
#' }
#' 

# Create urls based on game_ids
gameid2url <- function(game_id=NULL) {
    suffix <- c("/inning/inning_all.xml", "/miniscoreboard.xml")
    # Create an emply list to hold the results of the loop.
    datalist=gidzlist=NULL
    # Loop through gids and convert them to urls.
    for(i in seq_along(game_id)) {
    root <- "http://gd2.mlb.com/components/game/"
    league <- str_sub(game_id[i], -5, -3)
    yyyy <- str_sub(game_id[i], 5, 8)
    mm <- str_sub(game_id[i], 10, 11)
    dd <- str_sub(game_id[i], 13, 14)
    mlb_url <- paste0(root, league, "/", "year_", yyyy, "/month_", mm, "/day_", dd, "/", game_id)
    datalist[i] <- mlb_url[i]

    }

    # Grab miniscoreboard first, so we an find any games that weren't played.
    minilist <- datalist
    
    for(i in seq_along(minilist)){
        minilist[i] <- paste0(minilist[i], "/miniscoreboard.xml")
    }
    # Validate urls for miniscoreboard.
    gamez=validurlz=invalid=NULL
    for(i in 1:length(minilist)){
        if(!isTRUE(validURL(minilist[i]))){
            invalid[i] <- minilist[i]        
        }
    }
    # Strip out any NAs.
    # Subset games not played out of minilist and out of base gids.
    invalid <- invalid[!is.na(invalid)]
    minilist <- minilist[minilist != invalid]
    invalid <- str_sub(invalid, 1, str_length(invalid)-19)
    datalist <- datalist[datalist != invalid]

    # Append suffix to base URLs. There is probbaly a more elegant way to do this, but this works for now...
    inningsalllist <- datalist
    innignshitlist <- datalist
    playerslist <- datalist
    gameslist <- datalist
    gameeventslist <- datalist
    
    for(i in seq_along(inningsalllist)){
        inningsalllist[i] <- paste0(inningsalllist[i], "/inning/inning_all.xml")
    }
    for(i in seq_along(innignshitlist)){
        innignshitlist[i] <- paste0(innignshitlist[i], "/inning/inning_all.xml")
    }
    for(i in seq_along(playerslist)){
        playerslist[i] <- paste0(playerslist[i], "/players.xml")
    }
    for(i in seq_along(gameslist)){
        gameslist[i] <- paste0(gameslist[i], "/game.xml")
    }
    for(i in seq_along(gameeventslist)){
        gameeventslist[i] <- paste0(gameeventslist[i], "/game_events.xml")
    }
    
    gidz <- c(inningsalllist, innignshitlist, minilist, playerslist, gameslist, gameeventslist)
    
    return(gidz)
}




    
