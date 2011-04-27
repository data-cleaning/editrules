#' Gaussian or Fourier-Motzkin elimination 
#' 
#' Eliminate variables from a set of linear restrictions of the form
#' \eqn{{\bf a}\cdot {\bf x} \odot b} with \eqn{\odot\in\{<,<=,==\}}
#' represented as an augmented matrix [A,b] with Fourier-Motzkin and/or Gaussian 
#' elimination. 
#' 
#' A variable is eliminated by exploiting an equality when possible, otherwise
#' Fourier-Motzkin elimination is performed to eliminate the variable from the set
#' of inequalities. An observation of Kohler (1967) is used to reduce the number of 
#' redundant rows, as well as obvious redundancies amounting to 0 < 1. A warning 
#' is emitted when the system becomes obviously unfeasable (1 < 0). No exact feasability
#' test is performed (as this has the same complexity as completely solving the system).
#'
#'
#' @param A Augmented \code{matrix} [A,b] with real coefficients
#' @param J Vector of column names or indices in A
#' @param renormalize If TRUE, the rows of A are renormalized by dividing them
#'  by their maximum absulute value after each elimination step.
#' @param operators Optional character vector with \code{"<"},\code{"<="} or \code{"=="} for each row of A. 
#' @param tol Tolerance used in checking for zero coefficients
#'
#' @references
#' D.A. Kohler (1967) Projections of convex polyhedral sets, Operational Research
#' Center Report , ORC 67-29, University of California, Berkely.
#' 
#' H.P. Williams (1986) Fourier's method of linear programming and its dual,
#' The American Mathematical Monthly 93, 681-695
#'
#' @example examples/fourierMotzkin.R
#' 
#'
#' @export
fourierMotzkin <- function(A, J=1, operators=NULL, tol=sqrt(.Machine$double.eps) , 
        renormalize=TRUE){
    # valid operators?
    if ( !all(operators %in% c("<","<=","==") ))
        stop("Invalid operator: only < and <= are allowed")
    if ( !is.null(operators) && length(operators) != nrow(A) )
        stop("Number of operators not equal to number of rows in system.")

    fm <- function(j){
    
        I <- operators == "==" & abs(A[,j]) > tol
        if (any(I)){ #solvable from an equality?
            i <- which(I)[1]
            p <- A[i,vars]/A[i,j]
            v <- A[i,-vars]
            A   <<- t(
                apply(A[-i,],1, 
                    function(a){
                        c(a[vars] - a[j]*p, a[-vars]| v )
                    }
                )
            )
            operators <<- operators[-i]       
            return(TRUE)
        }

        # j does not occur in equality, eliminate from inequalities. 
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
            ), operators[iNot])
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
            redundant <- isObviouslyRedundant(A,operators,tol) |  rowSums(A[,-vars,drop=FALSE]) > nEliminated + 1
            if (any(redundant)){
                A <- A[!redundant,,drop=FALSE]
                operators <- operators[!redundant]
            }
            if ( renormalize ) A[,vars] <- t(apply(A[,vars,drop=FALSE],1,function(a) a/max(abs(a))))
        }  
    }
    if (isObviouslyUnfeasable(A, operators, tol=tol)) warning("System is unfeasable")
    if ( is.null(operators) ){
        return(A[,vars,drop=FALSE])
    } else {
        return(list(A=A[,vars,drop=FALSE],operators=operators))
    }
}


#' Check for obvious contradictions in set of (in)equalities
#' 
#' If any of the rows of the system Ax <operators> b is obviously constradictory the function 
#' returns TRUE, otherwise FALSE. Obvious inconsistencies may arise during elimination processes.
#' 
#' @param A Augmented matrix [A,b]
#' @param operators character vector with elements \code{"<"}, \code{"<="} or \code{"=="}. 
#' @param tol Tolerance for checking against zero.
#'
#' @nord
isObviouslyUnfeasable <- function(A, operators = NULL, tol=sqrt(.Machine$double.eps)){
    if ( is.null(operators) ) return(FALSE)
    b <- ncol(A)
    zeroCoef <- rowSums(abs(A[,-b,drop=FALSE])) < tol        
    if ( any(zeroCoef & operators %in% c("<", "<=") &  A[,b,drop=FALSE] < -tol) || 
         any(zeroCoef & operators == c("==") &  abs(A[,b,drop=FALSE]) > tol)) return(TRUE)
    return(FALSE)
}


#' Find obvious redundancies in set of (in)equalities
#'
#' The function returns a logical vector which is TRUE at any row of the system 
#' Ax <operators> b which is obviously redundant. Obvious redundancies may arise
#' durining elimination processes.
#' 
#' @param A Augmented matrix [A,b]
#' @param operators character vector with elements \code{"<"}, \code{"<="} or \code{"=="}. 
#' @param tol Tolerance for checking against zero.
#'
#' @nord
isObviouslyRedundant <- function(A, operators=NULL, tol=sqrt(.Machine$double.eps)){
    if ( is.null(operators) ) return(logical(nrow(A)))
    b <- ncol(A)
    zeroCoef <- rowSums(abs(A[,-b,drop=FALSE])) < tol
    return(as.vector(
        zeroCoef & operators %in% c("==","<=")  & abs(A[,b,drop=FALSE]) < tol |
        zeroCoef & operators %in% c("<", "<=")  & A[,b,drop=FALSE] > tol
    ))
}

