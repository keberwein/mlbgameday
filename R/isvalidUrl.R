#' @title validURL
#' @description A utility function to run a tryCatch on a URL.
#' @param target url
#' @export

validURL <- function(target) {  
    tryCatch({  
        con <- url(target)  
        a  <- capture.output(suppressWarnings(readLines(con)))  
        close(con)  
        TRUE;  
    },  
    error = function(err) {  
        occur <- grep("cannot open the connection", capture.output(err));  
        if(length(occur) > 0) FALSE;  
    }  
    )  
}  