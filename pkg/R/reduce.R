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



#' Reduce an editmatrix by substituting a variable
#'
#' Given a set of linear restrictions \eqn{E: {\bf Ax}\odot {\bf b}} with \eqn{\odot\in\{<,\leq,==\}},
#' and matrix \eqn{{\bf A}} with columns \eqn{{\bf a}_1,{\bf a}_2,\ldots,{\bf a}_n}.
#' Substituting variable \eqn{x_j} with a value \eqn{\tilde{\bf x}_j} means setting \eqn{{\bf a}_j=0}
#' and \eqn{{\bf b}={\bf a}_j\tilde{x}_j}.
#'
#' Note that the resulting \code{\link{editmatrix}} may be inconsistent because of inconsistencies in
#' \eqn{\tilde{\bf x}}.
#' 
#' @param E \code{editmatrix} object
#' @param var \code{character} with name of variable
#' @param value \code{numeric} with value of variable
#' @return reduced edit matrix 
#'
#' @export
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



