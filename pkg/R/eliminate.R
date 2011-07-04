#' Eliminate a variable from an editmatrix
#'
#' Uses Fourier-Motzkin elimination to eliminate a variable from a set
#' of linear (in)equality restrictions, represented as an \code{\link{editmatrix}}.
#' An observation of Kohler (1967) is used to reduce the number of implied 
#' restrictions. Obvious redundancies of the type 0 < 1 are removed as well. 
#'
#'
#' @param E an object of class \code{\link{editmatrix}}
#' @param var \code{character} name of the variable to eliminate
#' @param fancynames \code{logical} If true, the derived restrictions have rownames derived from the original restrictions (slower).
#' @return An editmatrix with an extra (hidden) attributes describing how the new restrictions were derived from the original ones.
#'    These attributes are used to remove redundancies when variables are repeatedly eliminated.
#' 
#' @example examples/eliminate.R
#'
#' @seealso \code{\link{editmatrix}} \code{\link{isObviouslyInfeasible}}
#' @references
#' D.A. Kohler (1967) Projections of convex polyhedral sets, Operational Research
#' Center Report , ORC 67-29, University of California, Berkely.
#' 
#' H.P. Williams (1986) Fourier's method of linear programming and its dual,
#' The American Mathematical Monthly 93, 681-695
#' @export
eliminateFM <- function(E, var, fancynames=FALSE){
    if (!isNormalized(E)) E <- normalize(E)
    vars <- getVars(E)

    if (!var %in% vars){
        warning("Trying to eliminate variable not occuring in E")
        return(E)
    }
    
    d <- attr(E,"derivedFrom")  
    n <- attr(E,"nEliminated")
    if ( is.null(d) ){
        n <- 0
        d <- matrix(FALSE,nrow=nrow(E),ncol=nrow(E))
        diag(d) <- TRUE 
        colnames(d) <- rownames(E)
    }
    
    
    m <- as.matrix(E)
    ops <- getOps(E)
    
    coefs <- m[,var]
    I <- coefs != 0
    
    eq <- I & (ops == "==")
    
    upper <- which(!eq & coefs > 0)
    lower <- which(!eq & coefs < 0)
    
    coefs[!eq] <- abs(coefs[!eq])
    
    eq <- which(eq);

    # elimination possible?
    if ( (length(upper) > 0 && length(lower) > 0) ||
         (length(eq) >= 1 && (length(upper) > 0 || length(lower) > 0)) ||
         (length(eq) >= 2) ){
       n <- n+1
    } else {
        return(E)
    } 


    #normalize matrix, every row has coefficient 1 or -1 for var
    m[I,] <- m[I,] / coefs[I]

    # eqs and ineqs w/coef>0 ==> ineqs w/coef<0
    equpper <- c(eq, upper)
    I1 <- rep(equpper,each=length(lower))
    I2 <- rep(lower, times=length(equpper))
    ml <- m[I1,,drop=FALSE] + m[I2,,drop=FALSE]
    ol <- ifelse(ops[I1] != "<", ops[I2], ops[I1])
    dl <- d[I1,,drop=FALSE] | d[I2,,drop=FALSE]

    # eqs ==> ineqs w/coef>0
    I1 <- rep(eq,each=length(upper))
    I2 <- rep(upper, times=length(eq))
    mu <- m[I2,,drop=FALSE] - m[I1,,drop=FALSE]
    ou <- ops[I2]
    du <- d[I1,,drop=FALSE] | d[I2,,drop=FALSE]

    # eqs ==> eqs
    me <- m[logical(0),,drop=FALSE]
    de <- d[logical(0),,drop=FALSE]
    if ( length(eq)>1){
        me <- t(t(m[eq[-1],,drop=FALSE]) - m[eq[1],])
        de <- t(t(d[eq[-1],,drop=FALSE]) | d[eq[1],])       
    } 
    oe <- rep("==",nrow(me))

    m <- rbind(ml,mu,me,m[!I,,drop=FALSE])
    d <- rbind(dl,du,de,d[!I,,drop=FALSE])
    o <- c(ol,ou,oe,ops[!I])
    redundant <- rowSums(d) > n + 1 | isObviouslyRedundant.matrix(m,o)

    m <- m[!redundant,,drop=FALSE]
    d <- d[!redundant,,drop=FALSE]
    if ( nrow(m) > 0 ){
        if ( fancynames ){
            rownames(m) <- paste("e",sapply(lapply(1:nrow(d),function(i) which(d[i,]) ),paste,collapse="."),sep="")
        } else {
            rownames(m) <- paste("e",1:nrow(m),sep="")
        }
    }
    neweditmatrix(
          m
        , o[!redundant]
        , normalized = TRUE 
        , derivedFrom = d 
        , nEliminated = n
    )
}


#' Check for obvious contradictions in set of (in)equalities
#' 
#' If any edit in E is an obvious contradictions of a form similar to 0 < -1, the function.
#' returns TRUE, otherwise FALSE. Obvious inconsistencies may arise during elimination processes.
#' 
#' @param E An normalized \code{link{editmatrix}}. If E is not normalized on entry, it will be normalized internally prior to checking. 
#' @param tol Tolerance for checking against zero.
#' @seealso \code{\link{eliminateFM}} \code{\link{editmatrix}}
#' @export
isObviouslyInfeasible <- function(E, tol=sqrt(.Machine$double.eps)){
    if ( !isNormalized(E) ) E <- normalize(E)
    A <- getAb(E)
    operators <- getOps(E)
    ib <- ncol(A)
    zeroCoef <- rowSums(abs(A[,-ib,drop=FALSE])) < tol  
    b <- round(A[,ib],ceiling(-log10(tol)))    
    if ( any(zeroCoef & operators == "<"    &  b <= 0) || 
         any(zeroCoef & operators == "<="   &  b <  0) || 
         any(zeroCoef & operators == c("==") &  abs(b) > tol)) return(TRUE)
    return(FALSE)
}



#' Find obvious redundancies in set of (in)equalities
#'
#' The function returns a logical vector which is TRUE at any row of the system 
#' Ax <operators> b which is obviously redundant. Obvious redundancies, amounting
#' to statements as 0==0 or 0 < 1 may arise durining elimination processes. The 
#' function also checks for duplicate rows in the augmented matrix [A|b]
#' 
#' Extra paramters:
#' \itemize{
#' \item{tol: tolerance to check for zeros, default square root of machine accuracy}
#' \item{duplicates: logical, check for duplicate rows?, default \code{TRUE}}
#' \item{duplicates.tol: tolerance for duplicate search, standard: \code{tol}}
#' }
#' @param E Augmented matrix A|b or editmatrix
#' @param ... parameters to be passed to other methods. 
#' 
#'
#'
#'
#'
#' @seealso \code{\link{isObviouslyRedundant.matrix}}, \code{\link{isObviouslyRedundant.editmatrix}}
#' @export 
isObviouslyRedundant <- function(E, ...){
    UseMethod("isObviouslyRedundant")
}

#' Redundancy check, \code{matrix} method
#'
#'
#' @method isObviouslyRedundant matrix
#'
#' @param E Augmented matrix [A|b] or \code{\link{editmatrix}}
#' @param ... parameters to be passed to other methods. 
#' @param operators character vecor of comparison operators in \code{<, <=, ==} of length \code{nrow(E)}
#' @param tol tolerance to check for zeros.
#' @param duplicates logical: check for duplicate rows?
#' @param duplicates.tol tolerance for duplicate search
#'
#' @seealso \code{\link{isObviouslyRedundant}}, \code{\link{isObviouslyRedundant.editmatrix}}
#' @S3method isObviouslyRedundant matrix
isObviouslyRedundant.matrix <- function(
    E, 
    operators, 
    tol=sqrt(.Machine$double.eps), 
    duplicates=TRUE, 
    duplicates.tol=tol,
    ... ){
    ib <- ncol(E)
    zeroCoef <- rowSums(abs(E[,-ib,drop=FALSE])) < tol
    v <- as.vector(
        zeroCoef & ( (operators %in% c("==","<=")  & abs(E[,ib]) < tol) 
                   | (operators %in% c("<", "<=")  & E[,ib] > tol)
                   )
    )
    if (duplicates){
        if ( duplicates.tol > 0 )  E <- round(E, ceiling(-log10(duplicates.tol)))
        v <- v | (duplicated.matrix(E) & duplicated.default(operators))
    }
    return(v)
}


#' Redundancy check, \code{editmatrix} method
#'
#'
#' @method isObviouslyRedundant editmatrix
#'
#' @param E Augmented matrix [A|b] or \code{\link{editmatrix}}
#' @param ... parameters to be passed to other methods. Currently, only \code{tol} is implemented (see \code{\link{isObviouslyRedundant.matrix}}).
#'
#' @seealso \code{\link{isObviouslyRedundant}}, \code{\link{isObviouslyRedundant.matrix}}
#'
#' @S3method isObviouslyRedundant editmatrix
isObviouslyRedundant.editmatrix <- function(E, ...){
    if ( !isNormalized(E) ) E <- normalize(E)
    isObviouslyRedundant.matrix(getAb(E),getOps(E), ...)
}

#' Check consistency of editmatrix 
#'
#' Applies fourier-motzkin elimination untill either all
#' variables are eliminated or the editmatrix becomes obviously
#' infeasible. The check rests on the theorem that a set of linear
#' inequalities is infeasible if and only if  0 < -1 can be derived from it. 
#'
#' @param E an \code{\link{editmatrix}}
#' @param warn logical: should a warning be raised when system is infeasible?
#' @return TRUE or FALSE
#'
#'
#' @export
isFeasible <- function(E, warn=TRUE){
    vars <- getVars(E)
    vars2 <- vars
    feasible <- TRUE
    while( feasible && length(vars) > 0 ){
        E <- eliminateFM(E,vars[1])
        vars <- vars[-1]
        feasible <- !isObviouslyInfeasible(E)
        if ( !feasible && warn )
            warning(
                paste("system becomes obviously infeasible after eliminating",
                paste(vars2[!(vars2 %in% vars)],collapse=", "))
            ) 
    }
    return(feasible)
}


#' Check for duplicate edit rules
#'
#' @param x an \code{\link{editmatrix}}
#' @param ... options to be passed to other methods
#' @return logical vector
#' @S3method duplicated editmatrix
#' @export
duplicated.editmatrix <- function(x,...){
    duplicated.matrix(getAb(x)) & duplicated.default(getOps(x))
}




