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
    #    for(s in seq_along(suffix)){
    #        gidz[i] <- paste0(mlb_url, suffix[s])
    #        datalist[i] <- gidz[i]
    #    }
    datalist[i] <- mlb_url[i]

    }
}

# Try the suffix as the outside loop. Also, may want to give lapply a shot here.
# Try this. Make copies of list and loop over those.
# inningslist <- datalist
# minilist <- datalist


    
