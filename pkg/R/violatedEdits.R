
#' Retrieve which rows of \code{data.frame dat} violate which constraints
#'
#' This is an S3 generic function for checking rows of a \code{data.frame} against
#' a number of edit restrictions. The edits can be entered either in \code{character}
#' \code{data.frame} or \code{editmatrix} format. The returned value is a logical matrix
#' with dimension (number of records )\eqn{times}(number of edits), indicating which
#' record violates (\code{TRUE}) which edit.
#'
#' This function can be used as an input for automatic corrections methods.
#' This method will fail if \code{E} contains variables that are not available in \code{dat}
#' 
#' @aliases violatedEdits.character violatedEdits.data.frame violatedEdits.editmatrix
#' @example ../examples/violatedEdits.R
#' @export
#' @seealso \code{\link{listViolatedEdits}}, \code{\link{checkRows}}
#' @param E \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked, if a named vector is supplied it will converted internally to a data.frame
#' @param ... further arguments that can be used by methods implementing this generic function
#' @return a logical matrix where each row indicates which contraints are violated
violatedEdits <- function(E, dat, ...){
    UseMethod("violatedEdits")
}


#' @rdname violatedEdits
#' @method violatedEdits character
#' @param name name of edits
#' @export
violatedEdits.character <- function(E, dat, name=NULL, ...){
    ed <- parseEdits(E)
    if (is.vector(dat) && !is.null(names(dat))){
       dat <- data.frame(t(dat))
    }
    M <- tryCatch(sapply(ed, eval, envir=dat), error=function(e){
        stop(paste("Not all edits can be evaluated, parser returned", e$message, sep="\n"))})
    if ( is.vector(M) )  M <- array(M, dim=c(1,length(M)))
    dimnames(M) <- list(record=rownames(dat),edit=names(E))
    return(!M)
}

#' Method for editmatrix
#'
#' For rules of the form Ax == b or Ax <= b, |Ax - b| <= tol is returned.
#' For rules of the form Ax < b, |Ax - b| < tol is returned. The editmatrix is normalized before
#' checks are performed.
#'
#' @rdname violatedEdits
#' @method violatedEdits editmatrix
#' @param tol tolerance to check rules against.
#' @export
violatedEdits.editmatrix <- function(E, dat, tol=sqrt(.Machine$double.eps), ...){
     if (tol < 0 ) stop("Argument tol must be nonnegative")
     if ( !isNormalized(E) ) E <- normalize(E)

     if ( is.vector(dat) ){ X <- t(dat) } else { X <- as.matrix(dat) }
     
     ops <- getOps(E)
     v <- abs(getA(E) %*% t(X) - getb(E))
     val <- matrix(logical(length(v)),nrow=nrow(v))
     I <- ops %in% c("==","<=")
     val[I,] <- v[I,,drop=FALSE] <= tol
     val[!I] <- v[!I,,drop=FALSE] < tol

     dimnames(val) <- list(edit=rownames(E),record=rownames(dat))
     !t(val)
}

#' @rdname violatedEdits
#' @method violatedEdits data.frame
#' @export
violatedEdits.data.frame <- function(E, dat, ...){
    if ( !all(c("name","edit","description") %in% names(E)) ){
        stop("Invalid input data.frame see ?editmatrix for valid input format")
    }
    return(violatedEdits.character(as.character(E$edit), dat, E$name))
}

#'
#' If \code{datamodel=TRUE} the first n edits (n being the number of variables) represent the datamodel.
#'
#' @method violatedEdits editarray
#' @param datamodel Also check against datamodel?
#' @rdname violatedEdits
#' @export
violatedEdits.editarray <- function(E, dat,datamodel=TRUE,...){
    edits <- as.character(E, useIf=FALSE, datamodel=datamodel)
    v <- violatedEdits.character(edits,dat,...)
#    dimnames(v) <- list(rownames(dat),names(edits))
    v
}



#' Lists which rows of \code{data.frame dat} violate which constraints
#'
#' This function can be used as an input for automatic corrections methods.
#' @example ../examples/listViolatedEdits.R
#' @export
#' @param E a number of edit restrictions, represented as \code{character} vector, \code{\link{editmatrix}} or \code{data.frame}.
#' @param dat \code{data.frame} with data that should be checked
#' @seealso \code{\link{violatedEdits}} \code{\link{checkRows}}
#' @return a list with per row a \code{integer} vector of the constraints that are violated 
listViolatedEdits <- function(E, dat){    
    errors <- violatedEdits(E, dat)
    errorlist <- apply(errors, 1, which)
    return(apply(errors, 1, which))
}








