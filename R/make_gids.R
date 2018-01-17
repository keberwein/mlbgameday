#' Create game ids from dates.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param league The league to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}.
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "linescore."
#' @param game_ids A list of user-supplied gameIds.
#' @param ... additional arguments
#' @importFrom purrr map
#' @importFrom stringr str_sub str_replace_all
#' @importFrom dplyr mutate filter
#' @importFrom utils data head tail
#' @import foreach
#' @keywords internal
#' @export
#' @examples
#'
#' gids <- make_gids(start = "2016-06-01", end = "2016-06-01", dataset="inning_all")
#' 
#' 

make_gids <- function(start=NULL, end=NULL, league="mlb", dataset=NULL, game_ids=NULL, ...) {

    root <- paste0("http://gd2.mlb.com/components/game/", league, "/")
    
    if(!is.null(game_ids)){
        game_ids <- paste0(root, "year_", stringr::str_sub(game_ids, 5, 8), "/month_", stringr::str_sub(game_ids, 10, 11), 
               "/day_", stringr::str_sub(game_ids, 13, 14), "/", game_ids)

        made_gids <- game_urls(game_ids, dataset = dataset)
    }
    
    if(!is.null(start) & !is.null(end)){
        if(as.Date(start) < as.Date("2008-02-26")) {
            warning("The mlbgameday package supports data beginning on '2008-03-26'. Please enter a valid start date")
        }
        #Format dates
        dateslist <- seq(as.Date(start), as.Date(end), by = "day")
        dates <- paste0("year_", format(dateslist, "%Y"), "/month_",
                        format(dateslist, "%m"), "/day_", format(dateslist, "%d"))
        
        # Check to see if gids within the start and end dates are in the internal dataset. If not, grab them.
        gidenv <- environment()
        data(game_ids, package = "mlbgameday", envir = gidenv)
        
        # Add a date column to gid data to make life easier.
        gid_dates <- dplyr::rename(game_ids, gid = gameday_link)
        last_date <- as.Date(tail(gid_dates$date_dt, 1))
        first_date <- as.Date(head(gid_dates$date_dt, 1))
        
        # If we've got the whole range of gids internally, just grab them and format.
        if(start >= first_date & end <= last_date){
            final_gids <- dplyr::filter(gid_dates, date_dt >= as.Date(start) & date_dt <= as.Date(end))
            final_gids$url <- paste0(root, "year_", stringr::str_sub(final_gids$date_dt, 1, 4), "/month_",
                                     stringr::str_sub(final_gids$date_dt, 6,7), "/day_", 
                                     stringr::str_sub(final_gids$date_dt, 9, 10),
                                     "/", final_gids$gid)
            final_gids <- final_gids$url %>% as.list()
        }
        
        # If we have no internal gids, the start date is greater than the last date in the internal data.
        if(start > last_date){
            # Find gap between the last_date in the gids and the date the user input.
            newgidz <- seq(as.Date(start), as.Date(end), by = "day")
            newdates <- paste0("year_", format(newgidz, "%Y"), "/month_",
                               format(newgidz, "%m"), "/day_", format(newgidz, "%d"))
            
    
            # Scrape the miniscoreboard for that day so we can extract game_id.
            final_gids <- validate_gids(newdates)
        }
        
        # If we have some at the start internally, but are missing end, grab the gids we have and format and grab anything missing.
        if(start < last_date & end > last_date){
            # Find gap between the last_date in the gids and the date the user input.
            gaplist <- seq(as.Date(start), as.Date(end), by = "day")
            gapdates <- paste0("year_", format(gaplist, "%Y"), "/month_",
                            format(gaplist, "%m"), "/day_", format(gaplist, "%d"))
            
            # Veryify those gids were games played. If played, scrape the miniscoreboard for that day so we can extract game_id.
            # This piece takes a while. It has to tryCatch every url.
            gapgids <- validate_gids(gapdates)
    
            # Get the other gids not in the end window.
            startgids <- filter(gid_dates, date_dt >= as.Date(start) & date_dt <= as.Date(last_date)) %>%
                mutate(gid = as.character(gid), date_dt = as.Date(date_dt))
                
            startgids$url <- paste0(root, league, "/", "year", str_sub(startgids$gid, 4, 8), "/", "month_",
                                    str_sub(startgids$gid, 10, 11), "/", "day_", str_sub(startgids$gid, 13, 14), 
                                    "/", startgids$gid)
            
            startgids <- select(startgids, url)
            
            final_gids <- c(startgids$gid, gapgids)
        }
        
        made_gids <- game_urls(final_gids, dataset = dataset)
    }
    
    return(made_gids)
}
