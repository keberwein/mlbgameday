#' Get Gameday data from MLBAM.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param league The league to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "linescore".
#' @param game_ids A list of user-supplied gameIds.
#' @param db_con A database connection from the \code{DBI} package.
#' @param overwrite Logical. Should current database be overwritten? Inherited from the \code{dbWriteTable} function from the \code{DBI} package.
#' The default value is FALSE.
#' @param ... additional arguments
#' @importFrom DBI dbWriteTable
#' @import utils
#' @export
#' @examples
#' 
#' \dontrun{
#' # Make a request for a single day.
#' df <- get_payload(start = "2016-06-01", end = "2016-06-01")
#' 
#' 
#' # Run larger requests in parallel.
#' library(doParallel)
#' library(foreach)
#' 
#' no_cores <- detectCores() - 2
#' cl <- makeCluster(no_cores) 
#' registerDoParallel(cl)
#' 
#' df <- get_payload(start = "2016-01-01", end = "2017-01-01")
#' 
#' stopImplicitCluster()
#' rm(cl)
#' 
#' }
#'
#' # Supply your own custom vector of game ids.
#' 
#' mygids <- search_gids(team = "indians", start = "2016-05-01", end = "2016-05-01")
#' 
#' df <- get_payload(game_ids = mygids)
#' 
#' 
get_payload <- function(start=NULL, end=NULL, league="mlb", dataset = NULL, game_ids = NULL, db_con = NULL, overwrite = FALSE, ...) {
    if(is.null(dataset)) dataset <- "inning_all"
    message("Gathering Gameday data, please be patient...")
    
    
    if(!is.null(game_ids)){
        urlz <- make_gids(game_ids = game_ids, dataset = dataset)
    }
    
    if(!is.null(start) & !is.null(end)){
        if(start < as.Date("2008-01-01")){
            stop("Please select a later start date. The data are not dependable prior to 2008.")
        }
        if(end >= Sys.Date()) stop("Please select an earlier end date.")
        
        if(start > end) stop("Your start date appears to occur after your end date.")
        start <- as.Date(as.character(start)); end <- as.Date(end); league <- tolower(league)
        # Get gids via internal function.
        urlz <- make_gids(start = start, end = end, dataset = dataset)
    }
    
    if(!is.null(db_con)){
        # Chunk out URLs in groups of 300 if a database connection is available.
        url_chunks <- split(urlz, ceiling(seq_along(urlz)/500))
        innings_df=NULL
        
        for(i in seq_along(url_chunks)){
            message(paste0("Processing data chunk ", i, " of ", length(url_chunks)))
            urlz <- unlist(url_chunks[i])
            if(dataset == "bis_boxscore") innings_df <- payload.gd_bis_boxscore(urlz)
            if(dataset == "game_events") innings_df <- payload.gd_game_events(urlz)
            if(dataset == "inning_all") innings_df <- payload.gd_inning_all(urlz)
            if(dataset=="inning_hit") innings_df <- payload.gd_inning_hit(urlz)
            if(dataset=="linescore") innings_df <- payload.gd_linescore(urlz)
            if(dataset=="game") innings_df <- payload.gd_game(urlz)
            
            if(isTRUE(overwrite)){
                for (i in names(innings_df)) DBI::dbWriteTable(conn = db_con, value = innings_df[[i]], name = i, overwrite = TRUE)
            }
            if(!isTRUE(overwrite)){
                for (i in names(innings_df)) DBI::dbWriteTable(conn = db_con, value = innings_df[[i]], name = i, append = TRUE)
            }

            # Manual garbage collect after every loop of 300 games.
            rm(innings_df); gc()
        }
        
        DBI::dbDisconnect(db_con)
        message(paste0("Transaction complete, disconnecting from the database.", " ", Sys.time()))
    }
    
    if(is.null(db_con)){
        # If no database connection, just return a dataframe.
        # If the returned dataframe looks like it's going to be large, warn the user.
        if(length(urlz) > 3500) { # One full season including spring training and playoffs is around 3000 games.
            if(utils::menu(c("Yes", "No"), 
                           title="Woah, that's a lot of data! Are you sure you want to continue without a database connection?")!=1){
                stop(message("Download stopped. Try a database connection or a smaller data set."))
            }else{ 
                message("Starting download, this may take a while...") 
            }
        }
        if(dataset == "bis_boxscore") innings_df <- payload.gd_bis_boxscore(urlz)
        if(dataset == "game_events") innings_df <- payload.gd_game_events(urlz)
        if(dataset == "inning_all") innings_df <- payload.gd_inning_all(urlz)
        if(dataset=="inning_hit") innings_df <- payload.gd_inning_hit(urlz)
        if(dataset=="linescore") innings_df <- payload.gd_linescore(urlz)
        if(dataset=="game") innings_df <- payload.gd_game(urlz)
        # Probably faster to do the transformation within the loop in cases where data gets very large.
        #innings_df <- transform_pload(innings_df)
        
        return(innings_df)
    }
}


