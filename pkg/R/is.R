

#' Check if object is an editmatrix
#' 
#' @seealso \code{\link{editmatrix}}
#' @export
#' @param x object to be checked
#' @return TRUE if \code{x} is an \code{editmatrix}
is.editmatrix <- function(x){
   return(inherits(x, "editmatrix"))
}

#' Check if object is a cateditmatrix
#' 
#' Please note that a cateditmatrix is also an editmatrix
#' @seealso \code{\link{cateditmatrix}}
#' @export
#' @param x object to be checked
#' @return TRUE if \code{x} is an \code{cateditmatrix}
is.cateditmatrix <- function(x){
  return(inherits(x, "cateditmatrix"))
}

#' check if an object is an editarray
#'
#' @param x object to be checked
#' @export
#' @return \code{TRUE} if \code{x} is an \code{\link{editarray}}
#' @seealso \code{\link{editarray}}, \code{\link{is.editmatrix}}
#'
is.editarray <- function(x) inherits(x,"editarray")
