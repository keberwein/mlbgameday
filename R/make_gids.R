#' Create game ids from dates.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param cluster A named parallel cluster produced by the \code{doParallel} package.
#' @param league The leauge to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}.
#' @param ... additional arguments
#' @importFrom purrr map
#' @importFrom stringr str_sub str_replace_all
#' @importFrom dplyr mutate filter
#' @import foreach
#' @export
#' @examples
#' \dontrun{
#' urls <- make_gids(start, end)
#' }
#' 

make_gids <- function(start=NULL, end=NULL, league="mlb", cluster=NULL, ...) {
    start <- as.Date(as.character(start)); end <- as.Date(end); league <- tolower(league)
    if(start < as.Date("2008-01-01")){
        stop(warning("Please select a later start date. The data are not dependable prior to 2008."))
    }
    if(end >= Sys.Date()-1){
        stop(warning("Please select an earlier end date."))
    }
    root <- paste0("http://gd2.mlb.com/components/game/", league, "/")
    #Format dates
    dateslist <- seq(as.Date(start), as.Date(end), by = "day")
    dates <- paste0("year_", format(dateslist, "%Y"), "/month_",
                    format(dateslist, "%m"), "/day_", format(dateslist, "%d"))
    
    # Check to see if gids within the start and end dates are in the internal dataset. If not, grab them.
    gidenv <- environment()
    data(game_ids, package = "tidygameday", envir = gidenv)
    
    # Add a date column to gid data to make life easier.
    gid_dates <- gid_date(game_ids) %>% mutate(gid = as.character(gid))
    
    last_date <- as.Date(tail(gid_dates$date_dt, 1))
    first_date <- as.Date(head(gid_dates$date_dt, 1))
    
    
    # This is probably 1000% broken.
    # If we've got the whole range of gids internally, just grab them and format.
    if(start >= first_date & end <= last_date){
        final_gids <- dplyr::filter(gid_dates, date_dt >= as.Date(start) & date_dt <= as.Date(last_date)) %>%
            dplyr::mutate(url = paste0("http://gd2.mlb.com/components/game/", league, "/", 
                                       "year_", format(date_dt, "%Y"), "/month_",
                                       format(date_dt, "%m"), "/day_", format(date_dt, "%d"), "/", gid)) %>% 
            select(url) %>% as.list()
    }
    
    
    # If we have some at the start internally, but are missing end, grab the gids we have and format and grab anything missing.
    if(end > last_date){
        # Find gap between the last_date in the gids and the date the user input.
        gaplist <- seq(as.Date(last_date), as.Date(end), by = "day")
        gapdates <- paste0("year_", format(gaplist, "%Y"), "/month_",
                        format(gaplist, "%m"), "/day_", format(gaplist, "%d"))
        
        #### The workflow breaks here. Need to combine the try_gids and validate_gis funcs maybe????
        
        # Veryify those gids were games played.
        # This piece takes a while. It has to tryCatch every url.
        gapgids <- validate_gids(gapdates, cluster = cluster)
        
        # Scrape gids from miniscorboards on those dates.
        #trygids <- try_gids(gapgids, cluster = cluster)
        
        # Get the other gids not in the end window.
        startgids <- filter(gid_dates, date_dt >= as.Date(start) & date_dt <= as.Date(last_date)) %>%
            mutate(gid = as.character(gid), date_dt = as.Date(date_dt))
        
        # If users start date is before the last_date in the gids, get thsoe gids to combine with the
        # gapgids. Otherwise the startgids df will be empty, which is fine.
        if(start < last_date){
        startgids$url <- paste0(root, league, "/", "year", str_sub(startgids$gid, 4, 8), "/", "month_",
                                    str_sub(startgids$gid, 10, 11), "/", "day_", str_sub(startgids$gid, 13, 14), 
                                    "/", startgids$gid)
        
        startgids <- select(startgids, url) %>% as.list()
        }
        
        final_gids <- c(startgids, gapgids)
        
    }
    return(final_gids)
}



#' Internal function to add a column of dates to a list of gids.
#' @param gidlist A list from the internal data set \code{game_id}
#' @param ... additional arguments.
#' @importFrom dplyr rename
#' @importFrom stringr str_replace str_sub
#' @keywords internal
#' @export


gid_date <- function(gidlist=NULL, ...){
    gidlist <- as.data.frame(gidlist)
    gidlist <- rename(gidlist, gid = gidlist)
    gidlist$date_dt <- stringr::str_sub(gidlist$gid, 5, 14) %>% stringr::str_replace_all("_", "-")
    return(gidlist)
}




