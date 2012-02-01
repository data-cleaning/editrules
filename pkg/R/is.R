

#' Check if object is an\code{\link{editmatrix}}
#' 
#' 
#' @export
#' @param x object to be checked
#' @return TRUE if \code{x} is an \code{editmatrix}
#' @seealso \code{\link{editarray}}, \code{\link{is.editset}}
is.editmatrix <- function(x){
   return(inherits(x, "editmatrix"))
}

#' Check if object is a \code{\link{cateditmatrix}}
#' 
#' Please note that a cateditmatrix is also an editmatrix
#' @seealso \code{\link{cateditmatrix}}
#' 
#' @param x object to be checked
#' @return TRUE if \code{x} is an \code{cateditmatrix}
#' @keywords internal
is.cateditmatrix <- function(x){
  return(inherits(x, "cateditmatrix"))
}

#' Check if an object is an \code{\link{editarray}}
#'
#' @param x object to be checked
#' @export
#' @return \code{TRUE} if \code{x} is an \code{\link{editarray}}
#' @seealso \code{\link{is.editset}}, \code{\link{is.editmatrix}}
#'
is.editarray <- function(x) inherits(x,"editarray")


#' Check if an object is an \code{\link{editset}}
#'
#' @param x object to be checked
#' @export
#' @return \code{TRUE} if \code{x} is an \code{\link{editset}}
#' @seealso \code{\link{editarray}}, \code{\link{is.editmatrix}}
#'
is.editset <- function(x) inherits(x,"editset")



