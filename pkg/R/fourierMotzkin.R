


#' Fourier motzkin elimination 
#' 
#' Eliminate variables from the real system Ax < b (or Ax <= b) by treating 
#' the augmented matrix [A,b] with Fourier-Motzkin elimination. A trick
#' of Kohler (1967) is used to reduce the number of redundant rows.
#'
#'
#' @param A Augmented \code{link{matrix}} [A,b] with real coefficients
#' @param J Vector of column names or indices in A
#' @param normalize If TRUE, the rows of A are renormalized by deviding them
#'  by their maximum absulute value after each elimination step.
#'
#' @references
#' D.A. Kohler (1967) Projections of convex polyhedral sets, Operational Research
#' Center Report , ORC 67-29, University of California, Berkely.
#' 
#' H.P. Williams (1986) Fourier's method of linear programming and its dual,
#' The American Mathematical Monthly 93, 681-695
#'
#'
fourierMotzkin <- function(A, J=1, normalize=TRUE){
# TODO take account of < ,<= and == 

	fm <- function(A, j, tol=sqrt(.Machine$double.eps)){
	
	    Apos <- A[A[,j] > tol, , drop=FALSE]
	    Azero <- A[abs(A[,j]) < tol,,drop=FALSE]
	    Aneg <- A[A[,j] < -tol, , drop=FALSE]
	
	    # nothing to eliminate?
	    if (nrow(Apos) == 0 || nrow(Aneg) == 0 ) 
	        return(A)    
	

	    Atop <- do.call(rbind, lapply(1:nrow(Aneg),function(i){   
	            aneg <- c(
	                Aneg[i,vars,drop=FALSE]/abs(Aneg[i,j]),
	                Aneg[i,-vars,drop=FALSE])
	            t(apply(Apos, 1, 
	                function(a) c(a[vars] + a[j]*aneg[vars], a[-vars] | aneg[-vars])
	            ))
	        })
	    )
        eliminated <<- TRUE
	    rbind(Atop[,,drop=FALSE],Azero)
	}
    
    vars <- 1:ncol(A)
    A <- cbind(A,diag(rep(1,nrow(A))))
    colnames(A)[-vars] <- rownames(A)
    nEliminated <- 0
    for ( j in J ){
        eliminated <- FALSE
        A <- fm(A[,,drop=FALSE], j)
        if ( eliminated ){ 
            nEliminated <- nEliminated+1
            redundant <- rowSums(A[,-vars,drop=FALSE]) > nEliminated + 1
            if (any(redundant)) A <- A[!redundant,,drop=FALSE]
            if ( normalize ) A[,vars] <- t(apply(A[,vars,drop=FALSE],1,function(a) a/max(abs(a))))
        }   
    }
    A[,vars,drop=FALSE]
}


#V <- matrix(rnorm(110),nrow=10,dimnames=list(
#        rules=paste(rep("e",10),1:10,sep=""),
#        variables=paste(rep("x",11),1:11,sep="")))

#E <- editmatrix(c(
#    "4*x1 - 5*x2 - 3*x3 + z <= 0",
#    "-x1 + x2 -x3 <= 2",
#    "x1 + x2 + 2*x3 <= 3",
#    "-x1 <= 0",
#    "-x2 <= 0",
#    "-x3 <= 0"))
#A <- cbind(getMatrix(E), getC(E))

    
#fourierMotzkin(A,1:2)







