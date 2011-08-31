
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


