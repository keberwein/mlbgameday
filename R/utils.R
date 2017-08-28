
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
NULL

#' Compound_pipe
#'
#' @name %<>%
#' @rdname compound_pipe
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @export
NULL

#' @title urlTrue
#' @description A utility function to run a tryCatch on a URL. Returns a logical TRUE / FALSE.
#' @param target url
#' @importFrom utils capture.output
#' @keywords internal
#' @export
urlTrue <- function(target) {  
    tryCatch({  
        con <- url(target)  
        a  <- capture.output(suppressWarnings(readLines(con)))  
        close(con)  
        TRUE;  
    },  
    error = function(err) {  
        occur <- grep("cannot open the connection", capture.output(err));  
        if(length(occur) > 0){
            close(con)
            FALSE;
        } 
    })  
}

#' @title comb
#' @description Internal combine fucntion for foreach loop used in get_payload()
#' @param x target
#' @param ... additional args
#' @importFrom purrr map
#' @keywords internal
#' @export
comb <- function(x, ...) {
    purrr::map(seq_along(x),
           function(i) c(x[[i]], purrr::map(list(...), function(y) y[[i]])))
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
