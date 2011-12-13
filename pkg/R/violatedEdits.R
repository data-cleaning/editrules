
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
    if (nrow(E)==0){
        v <- matrix( logical(0)
                   , nrow=ifelse(is.vector(dat),1,nrow(dat))
                   , ncol=0
                   )
        dimnames(v) <- list(record=rownames(dat),edit=NULL)
        return(newviolatedEdits(v))
    }
    if (tol==0) return(violatedEdits.character(as.character(E),dat))
    if ( !isNormalized(E) ) E <- normalize(E)

    eq <- getOps(E) == '=='
    iq <- !eq
    v <- matrix(FALSE,
        nrow=ifelse(is.vector(dat),1,nrow(dat)),
        ncol=nrow(E))

    if ( any(iq) ){
        E[iq,ncol(E)] <- E[iq,ncol(E)] + tol 
        v[,iq] <- violatedEdits.character(as.character(E[iq,]),dat)[,,drop=FALSE]
    }
    if ( any(eq) ){
        nc <- ncol(E)    

        Emin <- E[eq,]
        Emin[,nc] <- Emin[,nc] - tol
        emin <- gsub('==','>=',as.character(Emin))

        Emax <- E[eq,]
        Emax[,nc] <- Emax[,nc] + tol    
        emax <- gsub('==','<=', as.character(Emax))
        v[,eq] <- violatedEdits.character(emin,dat) | violatedEdits.character(emax,dat)
    }

    dimnames(v) <- list(record=rownames(dat),edit=rownames(E))
 
    newviolatedEdits(v)
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

#'
#'
#' @method violatedEdits editarray
#' @rdname violatedEdits
#' @export
violatedEdits.editset <- function(E, dat,datamodel=TRUE,...){
  Ea <- c(E$cat, E$mixcat)
  print(Ea)
  vCat <- violatedEdits(Ea, adddummies(E, dat), ...)
  vNum <- violatedEdits(E$num, dat, ...)
  #    dimnames(v) <- list(rownames(dat),names(edits))
  newviolatedEdits(cbind(vCat, vNum))
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
#' @param x \code{violatedEdits} object.
#' @param topn Top \code{n} edits to be plotted.
#' @param ... parameters passed to \code{barplot} method.
#' @export
plot.violatedEdits <- function(x, topn=min(10,ncol(x)), ...){
  N <- nrow(x)
  Nna <- sum(apply(is.na(x),1, all))

  editfreq <- sort(colSums(x, na.rm=TRUE), decreasing=TRUE)
  editfreq <- editfreq[1:topn]
  editfreq <- editfreq/N

  xlim <- c(0, max(editfreq))

  oldpar <- par(mfrow=c(2,1))
  barplot( sort(editfreq, decreasing=TRUE), 
         , main=paste("Edit violation frequency of top",topn,"edits")
         , xlab = "Frequency"
         , ylab= "Edit"
         , horiz = TRUE
         , las = 1
         , xlim = xlim
         , ...
         )
  
  x[is.na(x)] <- TRUE
  cnt <- table(rowSums(x))
  ner <- as.integer(names(cnt))
  cnt <- as.integer(cnt)
  
  noerr <- ner==0
  nnoer <- sum(cnt[noerr],0)
  ner <- ner[!noerr]
  cnt <- cnt[!noerr]
  lgcrit <- 50
  lg <- ''
  if ( max(ner) > lgcrit ) lg <- paste(lg,'x',sep='')
  if ( max(cnt) > lgcrit ) lg <- paste(lg,'y',sep='')
  plot( ner,cnt,
         , main=  "Edit violations per record"
         , xlab = "Number of violations"
         , ylab = "Count"
         , log=lg
         , ...
         )
  mtext(paste(nnoer,'records with no violations'),side=3,line=0)
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
  
  cat("Edit violations, ",N," observations, ",Nna," completely missing (",round(100*Nna/N,1),"%):\n\n", sep="")
  print(editdf, row.names=FALSE)
  
  object[is.na(object)] <- TRUE
  errfreq <- unclass(table(rowSums(object)))
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

#' as.data.frame violatedEdits
#' @method as.data.frame violatedEdits
#' @param x violatedEdits
#' @param ... not used
#' @export
as.data.frame.violatedEdits <- function(x, ...){
  as.data.frame(unclass(x),...)
}
