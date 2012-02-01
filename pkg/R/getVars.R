
#' get names of variables in a set of edits
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param ... Arguments to be passed to or from other methods
#' @seealso \code{\link{getA}}, \code{\link{getb}}, \code{\link{getOps}}
#' @example ../examples/getVars.R
#' @return \code{character} vector with the names of the variables. 
#' @export
getVars <- function(E,...){
    UseMethod("getVars")
}


#' Returns the variable names of an (in)equality \code{editmatrix} E
#'
#' @export
#' @method getVars editmatrix
#' @keywords internal
getVars.editmatrix <- function(E,...){
  colnames(E)[-ncol(E)]
}

#' Returns the variable names of an (in)equality \code{editmatrix} E
#'
#' @export
#' @method getVars editmatrix
#' @keywords internal
getVars.cateditmatrix <- function(E,...){
  unique(sub(":.+", "", colnames(E)[-ncol(E)]))
}

#' get variable names in editarray
#'
#' @export
#' @method getVars editarray
#' @keywords internal
getVars.editarray <- function(E,...) names(attr(E,"ind"))

#' get variable names in editset
#'
#' @param type which variables? \code{all} means all (except dummies), \code{num} means 
#'      all numericals, \code{cat} means all categoricals, \code{mix} means those numericals appearing in a logical 
#'      constraint and \code{dummy} means dummy variables connecting the logical with numerical constraints.
#' @param dummies Also return dummy variables used in \code{E$mixcat} (only if \code{type} is \code{all} or \code{cat})
#' @export
#' @method getVars editset
#' @keywords internal
getVars.editset <- function(E, type=c('all','num','cat','mix','dummy'), ...){
    type <- match.arg(type)
    numvars <- c()
    catvars <- c()

    if (type %in% c('all','num')){
        numvars <- unique(c(getVars(E$num), getVars(E$mixnum)))
    }
    if ( type == 'mix' ) numvars <- getVars(E$mixnum)
    if ( type %in% c('all','cat')){
        catvars <- getVars(E$mixcat)
        catvars <- catvars[!catvars %in% rownames(E$mixnum)]
    }
    if ( type == 'dummy'){
        catvars <- rownames(E$mixnum)
    }
    c(numvars, catvars)
}


