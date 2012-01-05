
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
    wd <- which(d)
    m <- nrow(A)
    if ( m == 0 ) return(logical(0))
    M <- (1:m)[!d]
    if ( length(M) == 1 ) return(d)
    m1 <- length(M)-1
    s <- logical(m)
    s[M] <- vapply(M, 
        function(i){
            I <- c(i,wd)
            any(rowSums(A[-I,,drop=FALSE] - (A[-I,,drop=FALSE] | A[rep(i,m1),,drop=FALSE]) ) == 0)
        },
        FUN.VALUE=FALSE 
    )
    s | d
}

# check if edits in A are subset of edits in B: (returns boolean vector)
isSubsetWrt.boolmat <- function(A,B){
    m <- nrow(A)
    n <- nrow(B)
    if ( m == 0 ) return(logical(0))
    if ( n == 0 ) return(rep(FALSE,n))

    vapply(1:m,function(i){
        any(rowSums(abs({A[rep(i,n),,drop=FALSE] | B} - B)) == 0)
    },FUN.VALUE=FALSE)
}





