
#' Reduce an (edit)matrix by removing empty rows and columns
#'
#' @keywords internal
#' @param E an object that is coerceable to a \code{matrix}
removeEmpty  <- function(E){
  m <- as.matrix(E)
  B <- m != 0
  vars <- (colSums(B) != 0)
  edits <- (rowSums(B) != 0)
  E[edits, vars, drop=FALSE]
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
        warning("Parameter var (", var[v==0], ") is not a variable of editmatrix E")
    }
    v <- v[v != 0]
    ib <- ncol(E)
    E[,ib] <- E[ ,ib] - E[ ,v]%*%value
    
    if (remove)
        E <- E[,-v, drop=FALSE]
    else 
        E[,v] <- 0
        
    E[!isObviouslyRedundant(E),]    
}

