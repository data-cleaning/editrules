
#' Check if any edit rule is a subset of another one
#'
#' @param E editarray
#' @return \code{logical} vector
#' @export
isSubset <- function(E){
    E <- getArr(E)
    # TODO: is this any faster with a while loop? Should we do this in C?
    d <- duplicated.editarray(E)
    m <- nrow(E)
    if ( m == 0 ) return(logical(0))
    M <- (1:m)[!d]
    m1 <- length(M)-1
    s <- sapply(M, function(i){
        any(rowSums(E[-i,,drop=FALSE] - (E[-i,,drop=FALSE]) | E[rep(i,m1),,drop=FALSE] ) == 0)
    })
    s | d
}


