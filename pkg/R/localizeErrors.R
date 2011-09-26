#' Localize errors on records in a data.frame.
#' 
#' Loops over all records in \code{dat} and performs error localization with \code{\link{errorLocalizer}}.
#' For each record it finds the smallest (weighted) number of variables to be imputed or adapted
#' such that all violated edits can be satisfied, without violating new ones. If there are multiple
#' optimal (equally weighted) solutions a random solution is chosen. 
#'
#'
#' @param E an object of class \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param dat a \code{data.frame} with variables in E.
#' @param ... options to be passed to \code{\link{errorLocalizer}}
#'
#' @return an object of class \code{errorLocation}
#' @example ../examples/localizeErrors.R
#' @export
localizeErrors <- function(E, dat, ...){
    stopifnot(is.data.frame(dat))
    user <- Sys.info()['user']
    call <- sys.call()

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
    for ( i in 1:n ){
        r <- X[,i]
        bt <- errorLocalizer(E,r,...)
        e <- bt$searchBest()
        if (!is.null(e) && !bt$maxdurationExceeded){
            err[i,] <- e$adapt
            weight[i] <- e$w
        }
        degeneracy[i] <- bt$degeneracy
        duration[i,] <- getDuration(bt$duration)
        maxDurationExceeded <- bt$maxdurationExceeded
    }
    structure(
        list(
            adapt  = err,
            status = data.frame(
                weight     = weight, 
                degeneracy = degeneracy, 
                duration,
                maxDurationExceeded
            ),
            call = call,
            user = user,
            timestamp = date()
        ),
        class=c('errorLocation','list')
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

#' Print object of class errorLocation 
#'
#' @param x object of class errorLocation
#' @param ... arguments to be passed to other methods
#' @method print errorLocation
#' @export
print.errorLocation <- function(x,...){
    cat("Object of class 'errorLocation' generated at",x$timestamp,'\n')
    cat("call :", as.character(as.expression(x$call)),'\n')
    cat("slots:",paste("$",names(x),sep=''),'\n\n')
    
    if ( nrow(x$adapt) <= 10 ){ 
        cat('Values to adapt:\n')
        print(x$adapt)
        cat('\n','Status:\n')
        print(x$status)
    } else {
        cat('Values to adapt:')
        print(x$adapt[1:10,,drop=FALSE])
        cat("...print truncated",'\n\n')
        cat('\n','Status:\n')
        print(x$status[1:10,,drop=FALSE])
        cat("...print truncated",'\n')
    }

}



