#' Field code forest algorithm
#'
#' Workhorse function for \code{\link{fcf}}
#'
#' @param E an editarray
#' @param vars variable names still to be eliminated from E
#' @param env an environment where all editmatrices will be stored
#'
#' @seealso \code{\link{fcf}}
#' keywords internal
fcf.env <- function(E, vars, env){
    assign(paste(vars,collapse='.'),E,envir=env)
    n <- length(vars)
    if ( n > 0 ) for ( i in 1:n ) fcf(eliminate(E,v[i]),vars[-i],env=env)
    env
}

#' Derive all essentially new implicit edits
#'
#' Implements the Field Code Forest algorithm of Garfinkel et al (1986) to 
#' derive all essentially new implicit edits from an editarray.
#'
#' @references
#' R.S. Garfinkel, A.S. Kunnathur and G.E. Liepins (1986). 
#'    Optimal imputation of erroneous data: categorical data, general edits.
#'    Operations Research 34, 744-751.
#'
fcf <- function(E){
    if ( !is.editarray(E) ) stop('Only for arguments of class editarray')
    as.list(fcf.env(E,getVars(E)))
}




