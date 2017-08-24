
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
#' @keywords internal
#' @export
comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}
