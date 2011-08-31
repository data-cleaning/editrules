
#' Check for obviously infeasible edit rules
#'
#' For linear edits, this function checks for obvious infeasibilities equivalent to 0 < -1.
#' For catgegorical edits, this function checks for edits wich have TRUE in all columns of the representation.
#' This corresponds to an espression stating that every possible value combination is erroneous.
#'
#' @param E An \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param ... Arguments to be passed to or from other methods.
#'
#' @export
isObviouslyInfeasible <- function(E,...){
    UseMethod("isObviouslyInfeasible")
}


#' Check for obvious contradictions in set of (in)equalities
#' 
#' @method isObviouslyInfeasible editmatrix
#' @param tol Tolerance for checking against zero.
#' @seealso \code{\link{eliminate}} \code{\link{editmatrix}}
#' @rdname isObviouslyInfeasible
#' @export
#' 
isObviouslyInfeasible.editmatrix <- function(E, tol=sqrt(.Machine$double.eps), ...){
    if ( !isNormalized(E) ) E <- normalize(E)
    A <- getAb(E)
    operators <- getOps(E)
    ib <- ncol(A)
    zeroCoef <- rowSums(abs(A[,-ib,drop=FALSE])) < tol  
    b <- round(A[,ib],ceiling(-log10(tol)))    
    if ( any(zeroCoef & operators == "<"    &  b <= 0) || 
         any(zeroCoef & operators == "<="   &  b <  0) || 
         any(zeroCoef & operators == c("==") &  abs(b) > tol)) return(TRUE)
    return(FALSE)
}

#' Check for obvious infeasibility
#' 
#' Check if boolean rep. of an edit is TRUE for every variable and every category.
#'
#' @method isObviouslyInfeasible editarray
#' @rdname isObviouslyInfeasible
#' @export
#' 
isObviouslyInfeasible.editarray <- function(E,...){
    any(rowSums(E)==ncol(E))
}





