
#' Coerce an editmatrix to a normal matrix
#' 
#' An \code{\link{editmatrix}} is a matrix and can be used as such, but it has extra attributes.
#' In some cases it is preferable to convert the editmatrix to a normal matrix.
#
#' Please note that coercion returns the augmented matrix \code{A|b} and not the \code{ops} part.
#'
#' @export
#' @method as.matrix editmatrix
#'
#' @param x editmatrix object
#' @param ... further arguments passed to or from other methods.
#'
#' @return augmented matrix of editmatrix
as.matrix.editmatrix <- function(x, ...){
   array(x, dim=dim(x), dimnames=dimnames(x))
}


#' Coerce an editarray to a boolean matrix
#' 
#' An \code{\link{editarray}} is a boolean array with extra attributes. The boolean
#' array may be extracted with this method.
#'
#' @export
#' @method as.matrix editarray
#'
#' @param x editarray object
#' @param ... further arguments to be passed to or from other methods
#'
#' @return boolean matrix of editarray.
as.matrix.editarray <- function(x,...){
    array(x,dim=dim(x),dimnames=dimnames(x))
}

