#' Gaussian or Fourier-Motzkin elimination 
#' 
#' Eliminate variables from a set of linear restrictions of the form
#' \eqn{{\bf a}\cdot {\bf x} \odot b} with \eqn{\odot\in\{<,<=,==\}}.
#' using Fourier-Motzkin and/or Gaussian 
#' elimination. If \code{E} contains other operators, it will be \code{\link{normalize}}d.
#' 
#' An observation of Kohler (1967) is used to reduce the number of 
#' redundant rows, as well as obvious redundancies amounting to 0 < 1. A warning 
#' is emitted when the system becomes obviously unfeasable (0 < -1). No exact feasability
#' test is performed (as this has the same complexity as completely solving the system).
#'
#' @aliases fourierMotzkin.editmatrix
#' @param E Linear set of edits, represented as an \code{\link{editmatrix}} or an augmented \code{matrix} [A,b] with real coefficients
#' @param variables Vector of column names or indices in E
#' @param renormalize If TRUE, the rows of A are renormalized by dividing them
#'  by their maximum absulute value after each elimination step.
#' @param tol Tolerance used in checking for zero coefficients
#' @param ... optional arguments. The argument \code{operators} (a character vector with \code{"<"},\code{"<="} or \code{"=="} for each row of E.) is
#'   obligated when calling \code{fourierMotzkin.matrix} explicitly
#'
#' @return If \code{E} is an editmatrix, an editmatrix with variables eliminated is returned, otherwise a list with
#'  elements \code{$E} (new augmented matrix), \code{$operators} (new operator set) is returned.
#'
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
fourierMotzkin <- function(E, variables=1, tol=sqrt(.Machine$double.eps), renormalize=FALSE, ...){
    UseMethod("fourierMotzkin")
}

#' @nord
#' @export
fourierMotzkin.editmatrix <- function(E, variables=1, tol=sqrt(.Machine$double.eps), renormalize=FALSE, ...){
#    if ( !isNormalized(E) ) E <- normalize(E)
    fm <- fourierMotzkin.matrix(E=as.matrix(E), variables,  tol, renormalize, operators=getOps(E),...)
    n <- ncol(E)
    as.editmatrix(fm$E[,1:(n-1),drop=FALSE],fm$E[,n],fm$operators) 
}

#' @nord
fourierMotzkin.matrix <- function(E, variables=1, tol=sqrt(.Machine$double.eps), renormalize=FALSE, operators,...){
    # internal function, performs a single elimination.
    fm <- function(j){
        eliminated <- FALSE
        iNot <- abs(E[,j]) < tol
        ENot <- E[iNot, ,drop=FALSE]
        
        iEq <- logical(nrow(E))
        if ( !is.null(operators) ) iEq <- operators == "=="

        # gaussian elimination step:
        Eeq <- E[!iNot &  iEq,,drop=FALSE]
        # equalities -> inequalities
        opin <- c()
        if ( any(iEq & !iNot) && any(!iEq & !iNot) ){
            Ein <- E[!iNot & !iEq,,drop=FALSE]
            Ein <- do.call(rbind, lapply(1:nrow(Eeq), function(i){
                v <- Eeq[i,-vars]
                a <- Eeq[i,vars]/Eeq[i,j]
                t(apply(Ein, 1,function(b) c(b[vars] - b[j]*a, v|b[-vars])))   
            }))
            opin <- rep(operators[!iNot & operators != "=="],nrow(Eeq))
            eliminated <- TRUE
        } 
        # equality -> equalities
        opeq <- c()
        if (nrow(Eeq) >= 2){
            v <- Eeq[1,-vars]
            a <- Eeq[1,vars]/Eeq[1,j]
            Eeq <- t(apply(Eeq[-1,,drop=FALSE], 1, function(b) c(b[vars] - b[j]*a, v | b[-vars] )))
            opeq <- rep("==", nrow(Eeq))
            eliminated <- TRUE
        } 
         

        # Fourier-Motzkin step inequalities -> inequalities
        iPos <- E[,j] > tol  & !iEq
        iNeg <- E[,j] < -tol & !iEq
        # nothing to eliminate?
        if ( any(iPos)  && any(iNeg) ){ 
            EPos <- E[iPos, , drop=FALSE]
            ENeg <- E[iNeg, , drop=FALSE]
            
            ETop <- do.call(rbind, lapply(1:nrow(ENeg),function(i){   
                aneg <- c(
                    ENeg[i,vars,drop=FALSE]/abs(ENeg[i,j]),
                    ENeg[i,-vars,drop=FALSE]
                )
                t(apply(EPos, 1, 
                    function(a) c(a[vars] + a[j]*aneg[vars], a[-vars] | aneg[-vars])
                ))
            }))
            eliminated <- TRUE
        }

        E <<- rbind(Eeq, Ein, ETop, ENot)
        operators <<- c(opeq, opin, as.vector(
            outer(operators[iNeg], operators[iPos], 
                function(o1,o2) ifelse(o1=="<=",o2,o1)
            )
        ), operators[iNot])
        return(eliminated)
    }
    
    vars <- 1:ncol(E)
    E <- cbind(E,diag(rep(1,nrow(E))))
    colnames(E)[-vars] <- rownames(E)
    nEliminated <- 0
    Eeq <- Ein <- ETop <- E[logical(0),,drop=FALSE]
    for ( j in variables ){
        if( fm(j) ){
            nEliminated <- nEliminated+1
#            redundant <- isObviouslyRedundant(E[,vars,drop=FALSE],operators,tol) |  
#                rowSums(E[,-vars,drop=FALSE]) > nEliminated + 1
#            E <- E[!redundant,,drop=FALSE]
#            operators <- operators[!redundant]
#            if ( renormalize ) E[,vars] <- t(apply(E[,vars,drop=FALSE],1,function(a) a/max(abs(a))))
        }  
    }
#    if (isObviouslyUnfeasable(E[,vars,drop=FALSE], operators, tol=tol)) warning("System is unfeasable")
    return(list(E=E[,vars,drop=FALSE],operators=operators))
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
isObviouslyUnfeasable <- function(A, operators, tol=sqrt(.Machine$double.eps)){
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
isObviouslyRedundant <- function(A, operators, tol=sqrt(.Machine$double.eps)){
    b <- ncol(A)
    zeroCoef <- rowSums(abs(A[,-b,drop=FALSE])) < tol
    return(as.vector(
        zeroCoef & operators %in% c("==","<=")  & abs(A[,b,drop=FALSE]) < tol |
        zeroCoef & operators %in% c("<", "<=")  & A[,b,drop=FALSE] > tol
    ))
}

