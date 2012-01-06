#' Field code forest algorithm
#'
#' Workhorse function for \code{\link{fcf}}
#'
#' @param E an editarray
#' @param vars variable names still to be eliminated from E
#' @param env an environment where all editmatrices will be stored
#'
#' @seealso \code{\link{fcf}}
#' @keywords internal
fcf.env <- function(E, vars, env=new.env()){
    if ( nrow(E) == 0 ) return(env)
    assign(paste(vars,collapse='.'),E,envir=env)
    n <- length(vars)
    if ( n > 0 ) for ( i in 1:n ) fcf.env(eliminate(E,vars[i]),vars[-i],env=env)
    env
}

#' Derive all essentially new implicit edits
#'
#' Implements the Field Code Forest algorithm of Garfinkel et al (1986) to 
#' derive all essentially new implicit edits from an editarray.
#'
#' @param E An \code{\link{editarray}}
#'
#' @references
#' R.S. Garfinkel, A.S. Kunnathur and G.E. Liepins (1986). 
#'    Optimal imputation of erroneous data: categorical data, general edits.
#'    Operations Research 34, 744-751.
#'
#' @export
fcf <- function(E){
    if ( !is.editarray(E) ) stop('Only for arguments of class editarray')
    as.list(fcf.env(E,getVars(E)))
}




