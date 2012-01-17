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
fcf.env <- function(E, env=new.env()){
    # if there are no edits left, we are done
    if ( nrow(E) == 0 ) return(env)

    # add edits to current set and remove redundant ones
    U <- c(env$E,E)
    env$E <- U[!isSubset(U),,drop=FALSE]

    # divide and conquer    
    B <- blocks(E)
    for (b in B){
        vars <- getVars(b)
        # eliminate most connected variables first
        vars <- vars[order(colSums(contains(b)), decreasing=TRUE)]
        # only loop over variables which can be eliminated by combining edits
        vars <- vars[resolves(b,vars)]
        # eliminate each variable one by one
        n <- length(vars)
        if ( n > 0 ) for ( i in 1:n ) FCF.env(reduce(eliminate(b,vars[i])),env=env)
    }
    env
}




#' Derive all essentially new implicit edits
#'
#' Implements the Field Code Forest algorithm of Garfinkel et al (1986) to 
#' derive all essentially new implicit edits from an editarray. At the moment
#' this algorithm has very little optimization and can be very slow.
#'
#' @param E An \code{\link{editarray}}
#'
#' @references
#' R.S. Garfinkel, A.S. Kunnathur and G.E. Liepins (1986). 
#'    Optimal imputation of erroneous data: categorical data, general edits.
#'    Operations Research 34, 744-751.
#'
fcf <- function(E){
    if ( !is.editarray(E) ) stop('Only for arguments of class editarray')
    e <- new.env()
    e$E <- E[integer(0),]
    fcf.env(E=E,env=e)$E
}



resolves <- function(E,vars){
    if ( length(vars)==0) return(logical(0))
    ind <- editrules:::getInd(E)[vars]
    Ic <- contains(E,vars)
    sapply(vars, function(v) all(colSums(E[Ic[,v],ind[[v]],drop=FALSE])>0))
}



