#' Retrieve readables editrules from an editmatrix
#'
#' THIS FUNCTION IS OBSOLETE AND WILL BE REMOVED. Use \link{as.data.frame} or \code{as.editmatrix} instead.
#' \code{editrules} returns a data.frame describing the editrules in editmatrix \code{x}. This data.frame can be used to store the
#' editrules in a readable format, so that the editrules can be maintained and documented.
#'
#' The \code{\link{editmatrix}} function can use the output of \code{editrules} to create an \code{editmatrix}.
#'
#' If \code{x} is a normal matrix, the matrix will be coerced to an \code{editmatrix}. The columns of the matrix
#' are the variables and the rows are the edit rules (constraints).
#' @example ../examples/editmatrixAttr.R
#' @aliases editrules
#' @seealso \code{\link{editmatrix}}
#' @export editrules
#' @param x \code{\link{editmatrix}}  or \code{matrix} object
#' @return \code{data.frame} with information on all edit rules / constraints
editrules <- function(x){
   warning("Function 'editrules' is obsolete and will be removed in the next release. Use as.data.frame instead")
   x <- as.editmatrix(x)
   edit <- as.character(x)
   er <- data.frame(name=names(edit), edit=edit, stringsAsFactors=FALSE)
   er$description <- attr(x, "description")
   er
}


#' Returns the constant part \code{b} of a linear (in)equality
#'
#' @example ../examples/editmatrixAttr.R
#' @export getb 
#' @seealso \code{\link{editmatrix}}
#'
#' @param E \code{\link{editmatrix}}
#' @return \code{numeric} vector \code{b}
getb <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  E <- unclass(E)
  E[,ncol(E)]
}



#' Returns the coefficient matrix \code{A} of linear (in)equalities
#'
#' @example ../examples/editmatrixAttr.R
#' @export getA 
#' @seealso \code{\link{editmatrix}}
#' @aliases getA 
#'
#' @param E \code{\link{editmatrix}}
#' @return \code{numeric} matrix \code{A}
getA <- function(E){
  if ( is.editmatrix(E) ){
    unclass(E)[,-ncol(E),drop=FALSE]
  } else {
     stop("E has to be an editmatrix")
  }
}


#' Returns augmented matrix representation of edit set.
#'
#' @example ../examples/editmatrixAttr.R
#' @seealso \code{\link{editmatrix}} \code{\link{as.matrix.editmatrix}}
#'
#' @param E \code{\link{editmatrix}}
#' @return \code{numeric} matrix \code{A|b}
#' @export 
getAb <- function(E){
    if (!is.editmatrix(E))  stop("E has to be an editmatrix.")
    unclass(E)[,,drop=FALSE]
}

#' Returns the operator part of a linear (in)equality \code{editmatrix} E
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example ../examples/editmatrixAttr.R
#' 
#' @param E \code{\link{editmatrix}}
#'
#' @return \code{character} vector with the (in)equality operators. 
getOps <- function(E){
  if (!is.editmatrix(E)){
     stop("E has to be an editmatrix.")
  }
  attr(E, "ops")
}


#' Check if an editmatrix is normalized
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example ../examples/editmatrixAttr.R
#' 
#' @param E \code{\link{editmatrix}}
#'
#' @return TRUE if editmatrix was normalized or created with \code{normalize=TRUE} 
isNormalized <- function(E){
  if (!is.editmatrix(E)){
     stop("Argument not of class editmatrix")
  }
  
  attr(E, "normalized") == TRUE ||
  all(getOps(E) %in% c("==","<","<="))
}

#' Normalizes an editmatrix
#'
#' An set of linear edits of the form \eqn{{\bf a}\cdot{\bf x}\odot b} with 
#' is called normalized when all  \eqn{\odot\in\{==,\leq,<\}} 
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @example ../examples/editmatrixAttr.R
#' 
#' @param E \code{\link{editmatrix}}
#'
#' @return If E was normalized, the original editmatrix is returned, otherwise 
#' a new normalized editmatrix will be returned 
normalize <- function(E){
  if (!is.editmatrix(E)) stop("Argument not of class editmatrix")
  if (isNormalized(E)){
     return(E)
  }
  
  A <- unclass(E)
  ops <- getOps(E)
  
  geq <- ops == ">="
  gt <- ops == ">"
  A[geq | gt,] <- -A[geq | gt,]
  ops[geq] <- "<="
  ops[gt] <- "<"      

  neweditmatrix(A, ops, normalized=TRUE)
}
