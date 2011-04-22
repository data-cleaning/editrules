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
#' @aliases editrules
#' @seealso \code{\link{editmatrix}}
#' @export editrules
#' @param x \code{\link{editmatrix}}  or \code{matrix} object
#' @return \code{data.frame} with information on all edit rules / constraints
editrules <- function(x){
   if (is.editmatrix(x)){
      return(attr(x, "editrules"))
   }
   editrules(as.editmatrix(x))
}

getC <- function(E){
  warning("This function is deprecated. Please use the function getb")
  getb(E)
}

#' Returns the constant part \code{b} of a linear (in)equality
#'
#' @example examples/editmatrixAttr.R
#' @aliases getb getC
#' @export getb getC
#' @seealso \code{\link{editmatrix}}
#'
#' @param E editmatrix
#' @return \code{numeric} vector \code{b}
getb <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  attr(E, "b")
}


getMatrix <- function(E){
  warning("This function is deprecated. Please use the function getA")
  getA(E)
}

#' Returns the coefficient matrix \code{A} of linear (in)equalities
#'
#' @example examples/editmatrixAttr.R
#' @export getA getMatrix
#' @seealso \code{\link{editmatrix}}
#' @aliases getA getMatrix
#'
#' @param E editmatrix
#' @return \code{numeric} matrix \code{A}
getA <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  unclass(E)[,,drop=FALSE]
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

#' Check if an editmatrix is normalized
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example examples/editmatrixAttr.R
#' 
#' @param E editmatrix
#'
#' @return TRUE if editmatrix was normalized or created with \code{normalize=TRUE} 
isNormalized <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  attr(E, "normalized") == TRUE
}

#' Normalizes an editmatrix
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example examples/editmatrixAttr.R
#' 
#' @param E editmatrix
#'
#' @return if E was normalized, the original editmatrix is returned, otherwise 
#' a new normalized editmatrix will be returned 
normalize <- function(E){
  if (isNormalized(E)){
     return(E)
  }
  A <- getMatrix(E)
  ops <- getOps(E)
  b <- getb(E)
  
  geq <- ops == ">="
  gt <- ops == ">"
  A[geq | gt,] <- -A[geq | gt,]
  b[geq | gt] <- -b[geq | gt]
  ops[geq] <- "<="
  ops[gt] <- "<"      

  as.editmatrix(A, b, ops)
}