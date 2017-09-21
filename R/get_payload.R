#' Get Gameday data from MLBAM.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param league The leauge to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "linescore".
#' @param game_ids A list of user-supplied gameIds.
#' @param ... additional arguments
#' @export
#' @examples
#' \dontrun{
#' df <- get_payload(url)
#' }
#' 
get_payload <- function(start=NULL, end=NULL, league="mlb", dataset = NULL, game_ids = NULL, ...) {
    start <- as.Date(as.character(start)); end <- as.Date(end); league <- tolower(league)
    if(is.null(dataset)) dataset <- "inning_all" 
    if(start < as.Date("2008-01-01")){
        stop("Please select a later start date. The data are not dependable prior to 2008.")
    }
    if(end >= Sys.Date()) stop("Please select an earlier end date.")
    
    if(start > end) stop("Your start date appears to occur after your end date.")
    # Get gids via internal function.
    urlz <- make_gids(start = start, end = end, dataset = dataset)
    
    if(dataset == "inning_all") innings_df <- payload.gd_inning_all(urlz)

    if(dataset=="linescore") innings_df <- payload.gd_linescore(urlz)
    
    return(innings_df)
}
