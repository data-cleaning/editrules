
#' Check if any edit rule is a subset of another one
#'
#' @param E editarray
#' @return \code{logical} vector
#' @export
isSubset <- function(E){
    if ( !is.editarray(E) ) stop('argument is not an editarray')
    isSubset.boolmat(getArr(E))
}

isSubset.boolmat <- function(A){
    if (nrow(A)==0) return(logical(0))
    if (nrow(A)==1) return(FALSE)

    d <- duplicated(A)
    m <- nrow(A)
    if ( m == 0 ) return(logical(0))
    M <- (1:m)[!d]
    if ( length(M) == 1 ) return(d)
    m1 <- length(M)-1
    s <- vapply(
        M, 
        function(i){
            any(rowSums(A[-i,,drop=FALSE] - (A[-i,,drop=FALSE]) | A[rep(i,m1),,drop=FALSE] ) == 0)
        },
        FUN.VALUE=FALSE 
    )
    s | d
}



