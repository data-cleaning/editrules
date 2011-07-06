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
    if ( sum(eq) <= 1 ) return(E)
    Ab <- getAb(E)
    Ab <- rbind(
        echelon.matrix(Ab[eq,,drop=FALSE]),
        Ab[!eq,,drop=FALSE]
    )
    neweditmatrix(Ab,c(o[eq],o[!eq]))

}

#' Write a system of equations in reduced row echelon form
#'
#'
#' @param E a matrix
#' @param tol tolerance for checking zero elements in E
#' @return the matrix in Reduced row echelon form
#' @nord
#' @export
echelon.matrix <- function(E, tol=sqrt(.Machine$double.eps), ...){
    k <- min(ncol(E),nrow(E))
    I <- 1:nrow(E)
    for ( i in 1:k ){
        I1 <- which(I >= i)
        ip <- I1[which.max(abs(E[I1,i]))]
        p <- E[ip,]
        if ( abs(p[i]) < tol ) next
        if ( ip > i ) E[c(ip,i),] <- E[c(i,ip),]
        E[-i,] <- E[-i,] - outer(E[-i,i],p/p[i])
    }
    d <- diag(E)
    id <- abs(d) > tol
    E[id,] <- E[id,]/d[id]
    I0 <- rowSums(abs(E) < tol) == ncol(E)
    rbind(E[!I0,,drop=FALSE],E[I0,,drop=FALSE])
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
#' @aliases replaceValue
#' @param E \code{editmatrix} object
#' @param var \code{character} with name(s) of variable(s)
#' @param value \code{numeric} with value(s) of variable(s)
#' @param remove \code{logical} should variable columns be removed from editmatrix?
#' @return reduced edit matrix 
#'
#' @export
substValue <- function(E, var, value, remove=FALSE){
    v <- match(var, getVars(E), nomatch=0)
    if (any(v==0)){
        stop("Parameter var (", var[v==0], ") is not a variable of editmatrix E")
    }
    
    ib <- ncol(E)
    E[,ib] <- E[ ,ib] - E[ ,v]%*%value
    
    E[,v] <- 0     
    if (remove){
        E[!isObviouslyRedundant(E),-v]
    }
    
    else {
        E[!isObviouslyRedundant(E),]
    }
}

#' @nord
#' @export
replaceValue <- function(...){
    stop("replaceValue is deprecated. Use substValue in stead.")
}

