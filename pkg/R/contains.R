#' determine which edits in an editmatrix contain a variable.
#'
#' For an \code{\link{editmatrix}}, variables with coefficients smaller than
#' \code{tol} are considered not to be contained in an edit.
#'
#' @param E \code{\link{editarray}} or \code{\link{editmatrix}}
#' @param var \code{character}, names of a categorical variables in \code{E}. If var=NULL, all variables are treated.
#' @param ... arguments to be passed to other methods
#' @return \code{logical} vector of length nrow(E), TRUE for edits containing \code{var}
#' @export
contains <- function(E,var=NULL,...){
    UseMethod('contains')
}

#' editmatrix method for contains
#' 
#' @method contains editmatrix
#' @rdname contains
#' @param tol tolerance to check zero-entries
#' @export
#' @keywords internal
contains.editmatrix <- function(E, var=NULL, tol=sqrt(.Machine$double.eps), ...){
    A <- getA(E)
    if (is.null(var)) var <- getVars(E)
    abs(A)[,var] > tol 
}

#' editarray method for contains
#'
#' @method contains editarray
#' @rdname contains
#' @export
contains.editarray <- function(E,var=NULL,...){
    if ( !is.editarray(E) ) stop("Argument not of class editarray")
    ind <- getInd(E)
    if ( is.null(var)) var <- names(ind)
   
    contains.boolmat(getArr(E),ind,var)
}

#' determine if a boolean matrix contains var
#'
#' @param A array
#' @param ind index
#' @param var variable name
#' @keywords internal
contains.boolmat <- function(A, ind, var){
    ind <- ind[var]
    v <- vapply(ind, function(ii) rowSums(A[,ii,drop=FALSE]) < length(ii), FUN.VALUE=logical(nrow(A)))
    if ( is.vector(v) ){ 
        v <- array(v,dim=c(1,length(v)), dimnames=list(edit=rownames(A),var=names(v)))
    } else {
        dimnames(v) <- list(edit=rownames(A),variable=colnames(v))
    } 
  v
}

