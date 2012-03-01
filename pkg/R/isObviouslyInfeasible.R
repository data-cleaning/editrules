
#' Check for obviously infeasible edit rules
#'
#' Find any obviously contradictory edit rules.
#'
#' @param E An \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param ... Arguments to be passed to or from other methods.
#' @return a \code{logical} oif the editset is obviously infeasible. A \code{logical} 
#'  vector in the case of an \code{\link[=disjunct]{editlist}} or \code{\link[=disjunct]{editset}}.
#' @export
isObviouslyInfeasible <- function(E,...){
    UseMethod("isObviouslyInfeasible")
}


#' Check for obvious contradictions in set of (in)equalities
#' 
#' Check for obvious infeasible rows, equivalent to 0 < -1.
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
#' For an \code{\link{editarray}}, it checks for edits wich have TRUE in all columns.
#' This corresponds to an espression stating that every possible value combination is erroneous.
#'
#' @method isObviouslyInfeasible editarray
#' @rdname isObviouslyInfeasible
#' @export
#' 
isObviouslyInfeasible.editarray <- function(E,...){
    any(rowSums(E)==ncol(E))
}

 
isObviouslyInfeasible.NULL <- function(E,...){
    FALSE
}


#' Check for obvious infeasibility
#' 
#' For an \code{\link{editset}} it checks infeasibility the numerical part,
#' contains an obvious infeasibility.
#'
#' @method isObviouslyInfeasible editset
#' @rdname isObviouslyInfeasible
#' @export
#' 
isObviouslyInfeasible.editset <- function(E,...){
    isObviouslyInfeasible(E$num) || isObviouslyInfeasible(E$mixcat)
}

#' Check for obvious infeasibility
#' 
#' For an \code{\link[=disjunct]{editlist}}, or \code{\link[=disjunct]{editenv}} 
#' each constituting \code{\link{editset}} is checked for obvious infeasibilities and 
#' returns a boolean vector of length \code{length(E)}.
#'
#' @method isObviouslyInfeasible editlist
#' @rdname isObviouslyInfeasible
#' @export
#' 
isObviouslyInfeasible.editlist <- function(E,...){
    vapply(E,isObviouslyInfeasible, FUN.VALUE=FALSE)
}

#'
#'
#'
#' @method isObviouslyInfeasible editenv
#' @rdname isObviouslyInfeasible
#' @export
#'
#'
isObviouslyInfeasible.editenv <- function(E,...){
    # note: environments are coerced to lists by lapply
    vapply(E,isObviouslyInfeasible, FUN.VALUE=FALSE)
}








