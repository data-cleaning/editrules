#' Reduce an (edit)matrix by removing empty rows and columns
#'
#' @nord
#' @param E an object that is coerceable to a \code{matrix}
removeEmpty  <- function(E){
  m <- as.matrix(E)
  B <- m != 0
  vars <- (colSums(B) != 0)
  edits <- (rowSums(B) != 0)
  E[edits, vars, drop=FALSE]
}

#' Partially reduce an editmatrix
#'
#'
#' DEFUNCT AND UNFINISHED!
#' This routine reduces an editmatrix in the following way:
#' \itemize{
#'  \item{The set of equalities are transformed to reduced row echelon form and rows 
#'        with zeros are removed}
#' \item{inequalities containing obvious truths (0<1) are removed}
#' }
#' 
#' 
#' matrix should be normalized, i.e. the operators should have similar sign
#' @nord
#' @param E normalized editmatrix object
#' @param tol tolerance
#' @return An \code{\link{editmatrix}} object, with linear redundant edits removed, or
#'      an empty \code{\link{editmatrix}} if E contains an inconsistent set of edits.
reduceMatrix <- function(E, tol){
    A <- getA(E)
    C <- getb(E)
    ops <- getOps(E)
      
    # reduced echelon form for equality edits
    eq <- ops == "=="
    Ared <- rref(cbind(A[eq,,drop=FALSE], C[eq]))
    C[eq] <- Ared[,ncol(Ared)]
    A[eq,] <- Ared[,1:(ncol(Ared)-1),drop=FALSE]
    # round near-zero coefficients
    A[!eq, ] <- ifelse(abs(A[!eq,,drop=FALSE]) < tol,0,A[!eq, ])
    C[!eq] <- ifelse( abs(C[!eq]) < tol, 0, C[!eq])

    # return empty editmatrix if inconsistency is encountered
    Azero <- colSums(A!=0) == 0
    
    consistent <- TRUE
    if (eq          & Azero & C != 0 ||
        ops == "<"  & Azero & C <= 0 ||
        ops == "<=" & Azero & C < 0)
        return(E[integer(0),])
    
    # remove tautologies
    tautology <- eq & Azero & C == 0 ||
        ops == "<"    & Azero & C >= 0 ||
        ops == "<="   & Azero & C > 0

    return(E[!tautology,])
}


#' Write a system of equations in reduced row echelon form
#'
#' This function is based on the code of John Fox, see
#' http://socserv.socsci.mcmaster.ca/jfox/Courses/R-programming/matrixDemos.R
#'
#' @param A a matrix
#' @return the matrix in Reduced row echelon form
#' @nord
rref <- function(A, tol=sqrt(.Machine$double.eps)){
    n <- nrow(A)
    m <- ncol(A)
    for (i in 1:min(c(m, n))){
        col <- A[,i]
        col[1:n < i] <- 0
    # find maximum pivot in current column at or below current row
        which <- which.max(abs(col))
        pivot <- A[which, i]
        if (abs(pivot) <= tol) next     # check for 0 pivot
        if (which > i) A[c(i, which),] <- A[c(which, i),]  # exchange rows
        A[i,] <- A[i,]/pivot            # pivot
        row <- A[i,]
        A <- A - outer(A[,i], row)      # sweep
        A[i,] <- row                    # restore current row
    }
    for (i in 1:n){
        if (max(abs(A[i,1:m])) <= tol)
            A[c(i,n),] <- A[c(n,i),] # 0 rows to bottom
    }
    A[abs(A) <= tol] <- 0
    return(A) 
}


#' Check consistency of editmatrix
#'
#' Check whether any record can obey the rules in an editmatrix
#' DEFUNCT AND UNFINISHED!
#' @param E editmatrix
#' @return TRUE or FALSE
#' @nord
is.consistent <- function(E){
    consistency = TRUE
    ops <- getOps(E)
    ineq <- ops != "=="
    eps <- sqrt(.Machine$double.eps)
    A <- getA(E)
    # check whether E has inconistent inequalities
    if ( any( colSums( abs(A[ineq,,drop=FALSE]) > eps ) == 0  & C[ineq] > 0 ) )
        consistency = FALSE
    # TODO check for inconsistencies in equalities


}



#' Reduce an editmatrix by setting a variable to a value
#'
#' @nord
#' @param E \code{editmatrix} object
#' @param var \code{character} with name of variable
#' @param value \code{numeric} with value of variable
#' @return reduced edit matrix or NULL if \code{value} is invalid with editmatrix
replaceValue <- function(E, var, value){
    v <- match(var, getVars(E), nomatch=0)
    if (v==0){
        stop("Parameter var (", var, ") is not a variable of editmatrix E")
    }
    
    ib <- ncol(E)
    E[ ,ib] <- E[ ,ib] - E[ ,v]*value
    E[,v] <- 0
    E
}



