
#' Remove empty rows and variables from set of edits
#'
#' If \code{E} is an \code{\link{editmatrix}} all rows and columns containing only zeros are removed.
#' If \code{E} is an \code{\link{editarray}} all variables not contained in any edit are removed. 
#' rows which have \code{FALSE} in every category of any variable are removed as well (these rows have
#' \code{\link{isObviouslyRedundant}} equal to \code{TRUE}).
#'
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param ... arguments to pass to other methods
#'
#' @export
reduce <- function(E,...){
    UseMethod('reduce')
}


#' 
#' @method reduce editmatrix
#' @param tol elements of \code{E} with absolute value < \code{tol} are considered 0.
#' 
#' @rdname reduce
#' @export
reduce.editmatrix  <- function(E, tol=sqrt(.Machine$double.eps),...){
 
  m <- as.matrix(E)
  if ( tol > 0 ) m[abs(m) < tol] <- 0
  B <- m != 0
  v <- 1:(ncol(m)-1)
  vars <- which(colSums(B[,v,drop=FALSE]) != 0)
  edits <- (rowSums(B) != 0)
  E[edits,c(vars,ncol(m)) , drop=FALSE]
}


#'
#' @method reduce editarray
#' 
#' @export
#' @rdname reduce
reduce.editarray <- function(E,...){
    E <- E[!isObviouslyRedundant.editarray(E),,drop=FALSE]
    m <- as.matrix(E)
    ind <- getInd(E)
    b <- sapply(ind,function(ind) all(m[,ind])) 
    if ( any(b) ){
        J <- logical(0)   
        for ( j in ind[b] ) J <- c(J,j)
        sep=getSep(E)
        m <- m[,-J,drop=FALSE]
        ind <- indFromArray(m,sep=sep)
        i <- apply(!m,1,all)
        m <- m[!i,,drop=FALSE]
        E <- neweditarray(E=m, ind=ind, sep=sep, names=rownames(m))
    }
    E
}






