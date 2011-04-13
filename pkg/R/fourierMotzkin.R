

#' @nord
fm <- function(A,j,tol=sqrt(.Machine$double.eps)){

    Apos <- A[A[,j] > tol, , drop=FALSE]
    Azero <- A[abs(A[,j]) < tol,,drop=FALSE]
    Aneg <- A[A[,j] < -tol, , drop=FALSE]
    # nothing to eliminate
    if (nrow(Apos) == 0 || nrow(Aneg) == 0 ) return(A)    

    aneg <- Aneg[1,,drop=FALSE]/abs(Aneg[1,j])
    Atop <- t(apply(Apos,1,function(a) a + a[j]*aneg ))

    apos <- Apos[1,,drop=FALSE]/Apos[1,j]
    Aneg <- t(apply(Aneg,1,function(a) a + abs(a[j])*apos))

    return(rbind(Atop,Azero,Aneg))

}

#' 
#'
#'
#'@nord
fourierMotzkin <- function(A){
    I <- 1:nrow(A)
    for ( j in 1:(min(nrow(A),ncol(A)-1)) ){
        A[I>j,] <- fm(A[I>j,,drop=FALSE],j)
    }
    A
}









