#' Fourier motzkin elimination 
#' 
#' Eliminate variables from the real system Ax < b (or Ax <= b) by treating 
#' the augmented matrix [A,b] with Fourier-Motzkin elimination. A trick
#' of Kohler (1967) is used to reduce the number of redundant rows.
#'
#'
#' @param A Augmented \code{link{matrix}} [A,b] with real coefficients
#' @param J Vector of column names or indices in A
#' @param normalize If TRUE, the rows of A are renormalized by dividing them
#'  by their maximum absulute value after each elimination step.
#' @param operators Optional character vector with < or <= for each row of A. 
#' @param tol Tolerance used in checking for zero coefficients
#'
#' @references
#' D.A. Kohler (1967) Projections of convex polyhedral sets, Operational Research
#' Center Report , ORC 67-29, University of California, Berkely.
#' 
#' H.P. Williams (1986) Fourier's method of linear programming and its dual,
#' The American Mathematical Monthly 93, 681-695
#'
#'
fourierMotzkin <- function(A, J=1, operators=NULL, tol=sqrt(.Machine$double.eps) , normalize=TRUE){
# TODO take account of  == 
        # valid operators?
    if ( !all(operators %in% c("<","<=") ))
        stop("Invalid operator: only < and <= are allowed")
    if ( !is.null(operators) && length(operators) != nrow(A) )
        stop("Number of operators not equal to number of rows in system.")

    fm <- function(j){
           
        iPos <- A[,j] > tol
        iNot <- abs(A[,j]) < tol
        iNeg <- A[,j] < -tol
        # nothing to eliminate?
        if (!any(iPos)  || !any(iNeg) )  return(FALSE)    
    
        APos <- A[iPos, , drop=FALSE]
        ANot <- A[iNot, ,drop=FALSE]
        ANeg <- A[iNeg, , drop=FALSE]
        
        ATop <- do.call(rbind, lapply(1:nrow(ANeg),function(i){   
            aneg <- c(
                ANeg[i,vars,drop=FALSE]/abs(ANeg[i,j]),
                ANeg[i,-vars,drop=FALSE]
            )
            t(apply(APos, 1, 
                function(a) c(a[vars] + a[j]*aneg[vars], a[-vars] | aneg[-vars])
            ))
        }))
        A <<- rbind(ATop[,,drop=FALSE],ANot)
        if ( !is.null(operators) ){
            operators <<- c(as.vector(
                outer(operators[iNeg], operators[iPos], 
                    function(o1,o2) ifelse(o1=="<=",o2,o1)
                )
            ), ops[iNot])
        }
        return(TRUE)
    }
    
    vars <- 1:ncol(A)
    A <- cbind(A,diag(rep(1,nrow(A))))
    colnames(A)[-vars] <- rownames(A)
    nEliminated <- 0
    for ( j in J ){
        if( fm(j) ){
            nEliminated <- nEliminated+1
            redundant <- rowSums(A[,-vars,drop=FALSE]) > nEliminated + 1
            if (any(redundant)){
                A <- A[!redundant,,drop=FALSE]
                operators <- operators[!redundant]
            }
            if ( normalize ) A[,vars] <- t(apply(A[,vars,drop=FALSE],1,function(a) a/max(abs(a))))
        }  
    }
    if ( is.null(operators) ){
        return(A[,vars,drop=FALSE])
    } else {
        return(list(A=A[,vars,drop=FALSE],operators=operators))
    }
}


V <- matrix(rnorm(110),nrow=10,dimnames=list(
        rules=paste(rep("e",10),1:10,sep=""),
        variables=paste(rep("x",11),1:11,sep="")))

E <- editmatrix(c(
    "4*x1 - 5*x2 - 3*x3 + z <= 0",
    "-x1 + x2 -x3 <= 2",
    "x1 + x2 + 2*x3 <= 3",
    "-x1 <= 0",
    "-x2 <= 0",
    "-x3 <= 0"))
A <- cbind(getMatrix(E), getC(E))

    
P2 <- fourierMotzkin(A,1:2)







