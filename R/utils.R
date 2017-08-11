
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
        if(length(occur) > 0) FALSE;  
    }  
    )  
}


#' @title unregister_cluster
#' @description Unregisters and closes a foreach cluster.
#' @param cl cluster
#' @importFrom parallel stopCluster
#' @export
unregister_cluster <- function(cl=NULL) {
    if(is_null(cl)){warning("Please specify the name you registered your foreach cluster with.")}
    stopCluster(cl)
    rm(cl)
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}
