#' Search the internal game_id data set.
#' @param team A team name, or character vector of team names, without the city, as found in the \code{team_ids} data set.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param venue The stadium at which the game(s) were played, as found in the \code{venue_ids} data set.
#' @param game_type The type of game(s), as found in the \code{game_ids} data set. Typical options include \code{"r"} (regular),
#' \code{"w"} (world series), \code{"l"} (league playoffs), \code{"d"} (division playoffs), and \code{"s"} (spring training).
#' @param home_only Logical. Collect only home games. The default is FALSE.
#' @param away_only Logical. Collect only away games. The default is FALSE.
#' @param ... additional arguments
#' @importFrom dplyr bind_rows filter
#' @importFrom stringr str_detect
#' @export
#' @examples
#' 
#' # Collect all of the regular season games for the 2016 Cleveland Indians.
#' game_ids <- search_gids(team = "indians", start = "2016-01-01", end = "2016-12-31", game_type = "r")
#' head(game_ids)
#' 
#' \dontrun{
#' # Collect all games from the 2016 World Series
#' game_ids <- search_gids(start = "2016-10-25", end = "2016-11-02", game_type = "w")
#' 
#' # Collect all regular season games played at Wrigley Field since 2008.
#' game_ids <- search_gids(venue = "Wrigley Field")
#' 
#' # Fuzzy search results work too.
#' game_ids <- search_gids(venue = "Wrigley")
#'
#' }


search_gids <- function(team=NULL, start=NULL, end=NULL, venue=NULL, game_type=NULL, home_only=FALSE, away_only=FALSE, ...) {
    # Check for user errors.
    if(isTRUE(home_only) & isTRUE(away_only)) {
        stop(message("home_only and away_only can not both be TRUE."))
    }
    
    game_ids <- mlbgameday::game_ids
    
    if(!is.null(team)){
        team <- upperfirst(team)
        home_games <- game_ids %>% dplyr::filter(stringr::str_detect(home_team_name, as.character(team)))
        away_games <- game_ids %>% dplyr::filter(stringr::str_detect(away_team_name, as.character(team)))
        game_ids <- dplyr::bind_rows(home_games, away_games)
    }
    
    if(!is.null(start)){
        start <- as.Date(start)
       game_ids %<>% dplyr::filter(as.Date(date_dt) >= start)
    }
    
    if(!is.null(end)){
        end <- as.Date(end)
        game_ids %<>% dplyr::filter(as.Date(date_dt) <= end)
    }
    
    if(!is.null(game_type)){
        game_t <- upperfirst(game_type)
        game_ids %<>% dplyr::filter(game_type == as.character(game_t))
    }
    
    if(!is.null(venue)){
        venue_n <- upperfirst(venue)
        game_ids %<>% dplyr::filter(stringr::str_detect(venue, as.character(venue_n)))
    }
    
    if(isTRUE(home_only)){
        team <- upperfirst(team)
        game_ids %<>% dplyr::filter(home_team_name == as.character(team))
    }
    
    if(isTRUE(away_only)){
        team <- upperfirst(team)
        game_ids %<>% dplyr::filter(away_team_name == as.character(team))
    }
    
    gids <- as.character(game_ids$gameday_link)
    
    return(gids)

}