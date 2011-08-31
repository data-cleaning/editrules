
#' get names of variables in a set of edits
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @seealso \code{\link{getA}}, \code{\link{getb}}, \code{\link{getOps}}
#' @example ../examples/getVars.R
#' @return \code{character} vector with the names of the variables. 
#' @export
getVars <- function(E){
    UseMethod("getVars")
}


#' Returns the variable names of an (in)equality \code{editmatrix} E
#'
#' @export
#' @method getVars editmatrix
#' @keywords internal
getVars.editmatrix <- function(E){
  colnames(E)[-ncol(E)]
}

#' get variable names in editarray
#'
#' @export
#' @method getVars editarray
#' @keywords internal
getVars.editarray <- function(E) names(attr(E,"ind"))

