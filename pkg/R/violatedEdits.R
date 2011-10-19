
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
    return(newviolatedEdits(!M))
}

#' Method for editmatrix
#'
#' \itemize{
#' \item{For rules of the form Ax == b  |Ax - b| <= tol is returned.}
#' \item{For rules of the form Ax < b, Ax - b < tol is returned.}
#' \item{For rules of the form Ax <= b Ax- b <= tol is returned.}
#'}
#' For numerical records, the default tolerance is 0. When working with doubles, 
#' the square root of machina accuracy is a resonable alternative (\code{sqrt(.Machine\$double.eps)}).
#' The editmatrix is \cite{\link[=normalize]{normalized}} before checks are performed.
#'
#' @rdname violatedEdits
#' @method violatedEdits editmatrix
#' @param tol tolerance to check rules against.
#' @export
violatedEdits.editmatrix <- function(E, dat, tol=0, ...){
    if (tol < 0 ) stop("Argument tol must be nonnegative")
    if ( !isNormalized(E) ) E <- normalize(E)
 
    if ( is.vector(dat) ) dat <- t(dat)    
    I <- match(getVars(E),colnames(dat),nomatch=0)
    X <- as.matrix(dat[,I,drop=FALSE]) 

    if (nrow(E) == 0){
        return(newviolatedEdits(matrix(logical(), nrow=NROW(dat))))
    }
     
    ops <- getOps(E)
    A <- getA(E)
    b <- getb(E)
    eq <- ops == "=="
    leq <- ops == "<="
    lt <- !(eq | leq)
    
    v <- matrix(FALSE,nrow=nrow(E),ncol=nrow(dat))
    v[eq,] <- abs( A[eq,,drop=FALSE]%*%t(X) - b[eq] ) <= tol
    v[leq,] <- A[leq,,drop=FALSE]%*%t(X) - b[leq] <= tol
    v[lt,] <- A[lt,,drop=FALSE]%*%t(X) - v[lt] < tol
    dimnames(v) <- list(edit=rownames(E),record=rownames(dat))
 
    newviolatedEdits(!t(v))
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
#'
#' @method violatedEdits editarray
#' @param datamodel Also check against datamodel?
#' @rdname violatedEdits
#' @export
violatedEdits.editarray <- function(E, dat,datamodel=TRUE,...){
    edits <- as.character(E, useIf=FALSE, datamodel=datamodel)
    v <- violatedEdits.character(edits,dat,...)
#    dimnames(v) <- list(rownames(dat),names(edits))
    newviolatedEdits(v)
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

newviolatedEdits <- function(x){
  structure(x, class="violatedEdits")
}

#' Plot summary statistics on violatedEdits
#' @method plot violatedEdits
#' @param x \code{violatedEdits} object
#' @param minfreq minimum frequency to be plotted
#' @param ... parameters passed to \code{barplot} method.
#' @export
plot.violatedEdits <- function(x, minfreq=1, ...){
  N <- nrow(x)
  Nna <- sum(apply(is.na(x),1, all))

  editfreq <- sort(colSums(x, na.rm=TRUE), decreasing=TRUE)
  editfreq <- editfreq[editfreq >= minfreq]
  editfreq <- editfreq/N

  x[is.na(x)] <- TRUE
  errfreq <- tabulate(1+rowSums(x)) / N
  names(errfreq) <- seq_along(errfreq) - 1
  
  xlim <- c(0, max(errfreq, editfreq))

  oldpar <- par(mfrow=c(2,1))
  barplot( sort(editfreq, decreasing=TRUE), 
         , main="Edit violation frequency"
         , xlab = "Frequency"
         , ylab= "Edit"
         , horiz = TRUE
         , las = 1
         , xlim = xlim
         , ...
         )
  
  barplot( errfreq
         , main="Observation violation frequency"
         , xlab = "Frequency"
         , ylab = "Number of violations"
         , horiz = TRUE
         , las= 1
         , xlim = xlim
         , ...
         )
  par(oldpar)
}

#' Summary statistics on violatedEdits
#'
#' @method summary violatedEdits
#' @param object \code{violatedEdits} object
#' @param E optional \code{editmatrix} or \code{editarray}
#' @param minfreq minimum freq for edit to be printed
#' @param ... (not used)
#' @export
summary.violatedEdits <- function(object, E=NULL, minfreq=1, ...){
  N <- nrow(object)
  Nna <- sum(apply(is.na(object),1, all))
  
  editfreq <- sort(colSums(object, na.rm=TRUE), decreasing=TRUE)
  editperc <- round(100*editfreq/N, 1)
  
  editperc <- editperc[editfreq >= minfreq]
  editfreq <- editfreq[editfreq >= minfreq]
  
  editdf <- data.frame(editname=names(editfreq), freq=editfreq, rel=paste(editperc,"%", sep=""))
  if (!is.null(E)){
     editdf$edit <- as.character(E)[editdf$editname]
  }
  
  cat("Edit violations (",N," observations, ",Nna," completely missing):\n\n", sep="")
  print(editdf, row.names=FALSE)
  
  object[is.na(object)] <- TRUE
  errfreq <- table(rowSums(object))/N
  names(errfreq) <- seq_along(errfreq) - 1
  errperc <- round(100*errfreq/(N), 1)
  errdf <- data.frame(errors=names(errfreq), freq=errfreq, rel=paste(errperc,"%", sep=""))
  cat("\nEdit violations per record:\n\n")
  print(errdf, row.names=FALSE)
}

#' Print violatedEdits
#' @method print violatedEdits
#' @param x violatedEdits
#' @param ... not used
#' @export
print.violatedEdits <- function(x,...){
  print(unclass(x))
}
