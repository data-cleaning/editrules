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


#' Bring an (edit) matrix to reduced row echelon form.
#'
#' If E is a matrix, a matrix in reduced row echelon form is returned.
#' If E is an \code{\link{editmatrix}} the equality part of E is transformed
#' to reduced row echelon form.
#'
#' @aliases echelon.editmatrix echelon.matrix
#'
#' @param E a matrix or editmatrix
#' @param ... options to pass on to further methods.
#' @export
echelon <- function(E,...){
    UseMethod("echelon")
}

#' @export
#' @nord
echelon.editmatrix <- function(E,...){
    o <- getOps(E)
    # nothing to eliminate?
    eq <- o == "=="
    if ( sum(eq) <= 1 ) return(E,...)
    Ab <- getAb(E)
    Ab <- rbind(
        echelon.matrix(Ab[eq,,drop=FALSE]),
        Ab[!eq,,drop=FALSE]
    )
    neweditmatrix(Ab,c(o[eq],o[!eq]))

}

#' Write a system of equations in reduced row echelon form
#'
#' This function is based on the code of John Fox, see
#' http://socserv.socsci.mcmaster.ca/jfox/Courses/R-programming/matrixDemos.R
#'
#' @param A a matrix
#' @return the matrix in Reduced row echelon form
#' @nord
#' @export
echelon.matrix <- function(E, tol=sqrt(.Machine$double.eps),...){
    n <- nrow(E)
    m <- ncol(E)
    for (i in 1:min(c(m, n))){
        col <- E[,i]
        col[1:n < i] <- 0
    # find maximum pivot in current column at or below current row
        which <- which.max(abs(col))
        pivot <- E[which, i]
        if (abs(pivot) <= tol) next     # check for 0 pivot
        if (which > i) E[c(i, which),] <- E[c(which, i),]  # exchange rows
        E[i,] <- E[i,]/pivot            # pivot
        row <- E[i,]
        E <- E - outer(E[,i], row)      # sweep
        E[i,] <- row                    # restore current row
    }
    for (i in 1:n){
        if (max(abs(E[i,1:m])) <= tol)
            E[c(i,n),] <- E[c(n,i),] # 0 rows to bottom
    }
    E[abs(E) <= tol] <- 0
    return(E) 
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
    E[,ib] <- E[ ,ib] - E[ ,v]*value
    E[,v] <- 0
    E[!isObviouslyRedundant(E),]
}