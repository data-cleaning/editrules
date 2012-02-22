#' Find obvious redundancies in set of edits
#' 
#' 
#' 
#' @param E Augmented matrix A|b, editmatrix 
#' @param duplicates \code{logical}: check for duplicate edits?
#' @param ... parameters to be passed to or from other methods. 
#' 
#' @return logical vector indicating which edits are (obviously) redundant
#'
#' @export 
isObviouslyRedundant <- function(E, duplicates=TRUE, ...){
    UseMethod("isObviouslyRedundant")
}


#' Redundancy check, \code{matrix} method
#'
#' 
#' Lower-level method, to be called directly from internal functions of the editrules package.
#'
#' @method isObviouslyRedundant matrix
#'
#' @param operators character vecor of comparison operators in \code{<, <=, ==} of length \code{nrow(E)}
#' @param tol tolerance to check for zeros.
#'
#' @rdname isObviouslyRedundant
#' @keywords internal
#' @seealso \code{\link{isObviouslyRedundant}}, \code{\link{isObviouslyRedundant.editmatrix}}
isObviouslyRedundant.matrix <- function(
    E, 
    operators, 
    tol=sqrt(.Machine$double.eps), 
    ... ){
    ib <- ncol(E)
    zeroCoef <- rowSums(abs(E[,-ib,drop=FALSE])) < tol
    as.vector(
        zeroCoef & ( (operators %in% c("==","<=")  & abs(E[,ib]) < tol) 
                   | (operators %in% c("<", "<=")  & E[,ib] > tol)
                   )
    )
}


#' Redundancy check for \code{editmatrix}
#'
#' Normalizes the editmatrix if necessary, converts it and passes it to the \code{matrix} method. All options
#' of the \code{matrix} method may be passed. Obvious redundancies, amounting
#' to statements as 0==0 or 0 < 1 will be detected, as well as duplicates.
#'
#' @method isObviouslyRedundant editmatrix
#' @rdname isObviouslyRedundant
#' 
#' @export
isObviouslyRedundant.editmatrix <- function(E, duplicates=TRUE, ...){
    if ( !isNormalized(E) ) E <- normalize(E)
    I <- isObviouslyRedundant.matrix(getAb(E), operators=getOps(E), ...)
    if ( duplicates ) I <- I | duplicated.editmatrix(E)
    I
}


#' Redundancy check for \code{editarray}
#'
#' Check if any of the variables has FALSE for every category (a record can never be contained in such a set).
#'
#' @method isObviouslyRedundant editarray
#' @rdname isObviouslyRedundant
#' @export
isObviouslyRedundant.editarray <- function(E, duplicates=TRUE, ...){
    if ( ncol(E) == 0 ) return(logical(0))
    if ( ncol(E) == 1 ) return(as.vector(E))
    ind <- getInd(E)
    red <- isRedundant.boolmat(getArr(E),getInd(E))
    if ( duplicates ) red <- red | duplicated.editarray(E)
    red
}


#' Check redundancy in editarray after disection
#'
#' @keywords internal
isRedundant.boolmat <- function(A, ind){
    if ( nrow(A) == 1 ) return(any(vapply(ind,function(i) sum(A[,i])==0,FUN.VALUE=TRUE)))
    apply(
        vapply(ind, function(i) rowSums(A[,i,drop=FALSE])==0, FUN.VALUE=logical(nrow(A))),
        1,any
    )
}



