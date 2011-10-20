#' Localize errors on records in a data.frame.
#' 
#' Loops over all records in \code{dat} and performs error localization with \code{\link{errorLocalizer}}.
#' For each record it finds the smallest (weighted) number of variables to be imputed or adapted
#' such that all violated edits can be satisfied, without violating new ones. If there are multiple
#' optimal (equally weighted) solutions a random solution is chosen. 
#'
#' For performance purposes, the edits are split in independent \code{\link{blocks}} which are processed
#' separately. The results are summarized in the output object, causing some loss of information.
#' For example, the number of solutions per record (degeneracy) per block is lost. To retain this 
#' information do someting like \code{err <- list(); for ( b in blocks(E)) err <- c(err,localizeErrors(b,dat))}
#'
#' 
#'
#' @param E an object of class \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param dat a \code{data.frame} with variables in E.
#' @param useBlocks process error localization seperatly for independent blocks in E?
#' @param verbose print progress to screen?
#' @param ... options to be passed to \code{\link{errorLocalizer}}
#'
#' @return an object of class \code{\link{errorLocation}}
#' @example ../examples/localizeErrors.R
#' @export
localizeErrors <- function(E, dat, useBlocks=TRUE, verbose=FALSE, ...){
    stopifnot(is.data.frame(dat))
    if ( !useBlocks ) return(localize(E,dat,...))
    B  <- blocks(E)
    n <- length(B)
    i <- 0
    blockCount <- NULL
    err <- NULL

    for ( b in B ){
        if ( verbose ){
            i <- i + 1
            blockCount <- paste('Processing block',format(i,width=nchar(n)), 'of',n)
        }

        err <- err %+% localize(b, dat, verbose, pretext=blockCount, call=sys.call(), ...)
    }
    if (verbose) cat('\n')
    err
}



#' Workhorse function for localizeErrors
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param dat \code{data.frame}
#' @param verbose \code{logical} print progress report during run?
#' @param pretext \code{character} text to print before progress report
#' @param call call to include in \code{\link{errorLocation}} object
#' 
#' @keywords internal
localize <- function(E, dat, verbose, pretext, call=sys.call(), weight=rep(1,ncol(dat)), ...){
## TODO: should we export this function?
    if ( any(is.na(weight)) ) stop('Missing weights detected')    
    weightperrecord <- FALSE
    if (is.array(weight)){
        if ( !all(dim(weight) == dim(dat)) ) stop("Weight must be vector or array with dimensions equal to argument 'dat'")
        weightperrecord <- TRUE
        W <- t(weight)
    } 
    if (!weightperrecord) wt <- weight
    

    n <- nrow(dat)
    m <- ncol(dat)
    err <- array(NA,
        dim=c(n,m),
        dimnames = list(
            record=rownames(dat),
            adapt=colnames(dat)
        )
    )   
    duration <- array(0,
        dim=c(n,3),
        dimnames=list(
            record = rownames(dat),
            duration = c('user','system','elapsed')
        )
    )
    weight <- rep(NA,n)
    degeneracy <- rep(NA,n)
    maxDurationExceeded <- logical(n)
    X <- t(dat)
    fmt <- paste('\r%s, record %',nchar(n),'d of %d',sep="")
    for ( i in 1:n ){
        if (verbose) cat(sprintf(fmt,pretext,i,n))  
        
        r <- X[,i]
        if (weightperrecord) wt <- W[,i]
        bt <- errorLocalizer(E, r, weight=wt, ...)
        e <- bt$searchBest()
        if (!is.null(e) && !bt$maxdurationExceeded){
            err[i,] <- e$adapt
            weight[i] <- e$w
        }
        degeneracy[i] <- bt$degeneracy
        duration[i,] <- getDuration(bt$duration)
        maxDurationExceeded <- bt$maxdurationExceeded
    }
    newerrorlocation(
        adapt=err,
        status = data.frame(
            weight  = weight,
            degeneracy=degeneracy,
            duration,
            maxDurationExceeded
            ),
        call=call,
        )
}

# code copied from print.proc.time...
getDuration <- function(x){
    y <- x
    if (!is.na(y[4L])) 
        y[1L] <- y[1L] + y[4L]
    if (!is.na(y[5L])) 
        y[2L] <- y[2L] + y[5L]
    y <- y[1L:3L]
    names(y) <- c(gettext("user"), gettext("system"), gettext("elapsed"))
    y
}


