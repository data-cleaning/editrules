#' Check consistency of edit set
#'
#' Applies Fourier-Motzkin elimination untill either all
#' variables are eliminated or the editmatrix becomes obviously
#' infeasible. The check rests on the theorem that a set of linear
#' inequalities is infeasible if and only if  0 < -1 can be derived from it. 
#'
#' @param E an \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param warn logical: should a warning be raised when system is infeasible?
#' @return TRUE or FALSE
#'
#'
#' @export
isFeasible <- function(E, warn=FALSE){
    ## TODO: make it return the subset of edits causing the contradiction.
    vars <- getVars(E)
    vars2 <- vars
    feasible <- !isObviouslyInfeasible(E)
    while( feasible && length(vars) > 0 ){
        E <- eliminate(E,vars[1])
        vars <- vars[-1]
        feasible <- !isObviouslyInfeasible(E)
        if ( !feasible && warn )
            warning(
                paste("system becomes obviously infeasible after eliminating",
                paste(vars2[!(vars2 %in% vars)],collapse=", "))
            ) 
    }
    return(feasible)
}






