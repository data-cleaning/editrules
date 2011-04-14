

#' @nord
fm <- function(A,j, tol=sqrt(.Machine$double.eps)){

    Apos <- A[A[,j] > tol, , drop=FALSE]
    Azero <- A[abs(A[,j]) < tol,,drop=FALSE]
    Aneg <- A[A[,j] < -tol, , drop=FALSE]

    # nothing to eliminate?
    if (nrow(Apos) == 0 || nrow(Aneg) == 0 ) 
        return(A)    
    
    Atop <- A[integer(0),]

    Atop <- do.call(rbind, lapply(1:nrow(Aneg),function(i){   
            aneg <- c(
                Aneg[i,vars,drop=FALSE]/abs(Aneg[i,j]),
                Aneg[i,-vars,drop=FALSE])
            t(apply(Apos, 1, 
                function(a) c(a[vars] + a[j]*aneg[vars], a[-vars] | aneg[-vars])
            ))
        })
    )
    redundant <- rowSums(Atop[,-vars,drop=FALSE]) > nEliminated + 1
    rbind(Atop[!redundant,,drop=FALSE],Azero)
}

#' 
#'
#'
#'@nord
fourierMotzkin <- function(A, J=1){
    vars <- 1:ncol(A)
    A <- cbind(A,diag(rep(1,nrow(A))))
    colnames(A)[-vars] <- rownames(A)
    nEliminated <- 0
    for ( j in J ){
        A <- fm(A[,,drop=FALSE],j)
        nEliminated <- nEliminated+1
    }
    A
}


#E <- editmatrix(c(
#    "4*x1 - 5*x2 - 3*x3 + z <= 0",
#    "-x1 + x2 -x3 <= 2",
#    "x1 + x2 + 2*x3 <= 3",
#    "-x1 <= 0",
#    "-x2 <= 0",
#    "-x3 <= 0"))
#A <- cbind(getMatrix(E), getC(E))

    
#fourierMotzkin(A,1:2)







