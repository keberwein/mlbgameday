library(mlbgameday)
library(testthat)

# First, make sure the link returns vailid as it should. The following tryCatch should return TRUE.
urlExists <- function(target) {  
    tryCatch({  
        con <- url(target)  
        a  <- capture.output(suppressWarnings(readLines(con)))  
        close(con)  
        TRUE;  
    },  
    error = function(err) {  
        occur <- grep("cannot open the connection", capture.output(err));  
        if(length(occur) > 0) {
            close(con)
            FALSE;
        }  
    })  
}

url_out <- urlExists("http://gd2.mlb.com/components/game/mlb/year_2016/month_05/day_01/gid_2016_05_01_clemlb_phimlb_1/inning/inning_all.xml")
expect_true(url_out)

# If we have a connection to the URL, we should be able to gather the paylaod.
if(isTRUE(url_out)){
    # Grab a single inning_all gid.
    mygids <- search_gids(team = "indians", start = "2016-05-01", end = "2016-05-01")
    df <- get_payload(game_ids = mygids)
    # Since this is an old game, we know what kind of structure to expect.
    expect_length(df, 5)
}


