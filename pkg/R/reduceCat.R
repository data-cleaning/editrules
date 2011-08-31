
#' check if any edit rule is a subset of another one)
#'
#' @param E editarray
#' @return \code{logical} vector
#' @export
isSubset <- function(E){
    E <- getArr(E)
    # TODO: is this any faster with a while loop? Should we do this in C?
    m <- nrow(E)
    if ( m == 0 ) return(logical(0))
    m1 <- m-1
    sapply(1:m, function(i){
        any(rowSums(E[-i,,drop=FALSE] - (E[rep(i,m1),,drop=FALSE] | E[-i,,drop=FALSE])) == 0)
    })
}


#' Substitute a value in an editarray: 
#'
#' @param E an editarray
#' @param var \code{character}, the variable name
#' @param value \code{character}, the value
#' @param ... other arguments to be passed to or from other methods
#' @return \code{\link{editarray}} with one variable substituted
#'
#' @export
substValue.editarray <- function(E, var, value, ...){
# TODO: make this work for multiple variables and values.
    J <- getInd(E)[[var]]
    sep=getSep(E)
    value <- paste(var,value,sep=sep)
    ival <- intersect(which(colnames(E) == value), J) 
    if ( length(ival) != 1 ) 
        stop(paste("Variable ", var,"not present in editarray or cannot take value",value))
    ii <- setdiff(J,ival)
    A <- getArr(E)
    A[,ii] <- FALSE
    I <- A[,ival]
    neweditarray(E=A[I,,drop=FALSE], ind=getInd(E), sep=sep, levels=getlevels(E))
}

