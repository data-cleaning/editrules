#' Retrieve readables editrules from an editmatrix
#'
#' \code{editrules} returns a data.frame describing the editrules in editmatrix \code{x}. This data.frame can be used to store the
#' editrules in a readable format, so that the editrules can be maintained and documented.
#'
#' The \code{\link{editmatrix}} function can use the output of \code{editrules} to create an \code{editmatrix}.
#'
#' If \code{x} is a normal matrix, the matrix will be coerced to an \code{editmatrix}. The columns of the matrix
#' are the variables and the rows are the edit rules (constraints).
#' @example examples/editmatrixAttr.R
#' @aliases editrules editsinfo
#' @seealso \code{\link{editmatrix}}
#' @export editrules editsinfo
#' @param x \code{\link{editmatrix}}  or \code{matrix} object
#' @return \code{data.frame} with information on all edit rules / constraints
editrules <- function(x){
   if (is.editmatrix(x)){
      return(attr(x, "editrules"))
   }
   editrules(as.editmatrix(x))
}

editsinfo <- function(x){
   warning("Deprecated function, please use editrules")
   editrules(x)
}

#' Returns the constant part of a linear (in)equality
#'
#' @example examples/editmatrixAttr.R
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @param E editmatrix
#' @return \code{numeric} vector \code{C}
getC <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  attr(E, "C")
}

#' Returns the operator part of a linear (in)equality \code{editmatrix} E
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example examples/editmatrixAttr.R
#' 
#' @param E editmatrix
#'
#' @return \code{character} vector with the (in)equality operators. 
getOps <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  attr(E, "ops")
}

#' Returns the variable names of an (in)equality \code{editmatrix} E
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example examples/editmatrixAttr.R
#' 
#' @param E editmatrix
#'
#' @return \code{character} vector with the names of the variables. 
getVars <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  colnames(E)
}