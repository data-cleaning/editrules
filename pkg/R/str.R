
#' \code{\link{str}} method for editmatrix object
#'
#' @method str editmatrix
#' @param object \code{\link{editmatrix}} object
#' @param ... arguments to pass to other methods
#' @export
str.editmatrix <- function(object,...){
    vars <- paste(getVars(object),collapse=", ")
    if (nchar(vars) > 20 ) vars <-  paste(strtrim(vars,16),"...") 
    cat(paste("editmatrix with", nrow(object), "edits containing variables",vars,"\n"))
}


#' \code{\link{str}} method for editarray object
#'
#' @method str editarray
#' @param object \code{\link{editarray}} object
#' @param ... arguments to pass to other methods
#' @export
str.editarray <- function(object,...){
    vars <- paste(getVars(object),collapse=", ")
    if (nchar(vars) > 20 ) vars <-  paste(strtrim(vars,16),"...") 
    cat(paste("editarray with", nrow(object), "edits containing variables",vars,"\n"))
}


