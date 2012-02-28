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
#' By default, all weights are set equal to one (each variable is considered equally reliable). If a vector 
#' of weights is passed, the weights are assumed to be in the same order as the columns of \code{dat}. By passing
#' an array of weights (same dimension as \code{dat}) separate weights can be specified for each record.
#'
#' If \code{E} is a \code{\link{editarray}}, variables not conforming to the datamodel specified vy \code{E}
#' will be detected as erroneous.
#'
#' @param E an object of class \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param dat a \code{data.frame} with variables in E.
#' @param useBlocks \code{DEPRECATED}. Process error localization seperatly for independent blocks in \code{E} (always \code{TRUE})?
#' @param verbose print progress to screen?
#' @param weight Vector of positive weights for every variable in \code{dat}, or 
#'      an array of weights with the same dimensions as \code{dat}.
#' @param method should errorlocalizer ("localizer") or mix integer programming ("mip") be used? NOTE: option "mip" is currently experimental. 
#' @param maxduration maximum time for \code{$searchBest()} to find the best solution for a single record.
#' @param ... Further options to be passed to \code{\link{errorLocalizer}}
#'  
#' @seealso \code{\link{errorLocalizer}}
#' @return an object of class \code{\link{errorLocation}}
#' @example ../examples/localizeErrors.R
#' @export
localizeErrors <- function(E, dat, verbose=FALSE, weight=rep(1,ncol(dat)), maxduration=600, method=c("localizer", "mip"), useBlocks=TRUE, ...){
    stopifnot(is.data.frame(dat))
    if ( any(is.na(weight)) ) stop('Missing weights detected')    

    if (is.array(weight) && !all(dim(weight) == dim(dat)) ) 
        stop("Weight must be vector or array with dimensions equal to argument 'dat'")

    weight <- array(weight,
        dim=c(
            ifelse(is.vector(weight),1,nrow(dat)),
            ncol(dat)
        )
    )
    if ( is.null(colnames(weight)) ) colnames(weight) <- names(dat)

    # convert logical and factor to character (except for complete NA-columns)
    dat <- data.frame(rapply(
            dat, f=function(x){
                if ( !all(is.na(x)) ){  
                    as.character(x)
                } else {
                    x
                }
            }, 
            classes=c('logical','factor'), 
            how='replace'
            ),
            stringsAsFactors=FALSE
    )
    # call mip method (no blocking necessary: this is done by lpSolve)
    if ( match.arg(method) == "mip" ){
        checklpSolveAPI()
        return(localize(E,dat,call=sys.call(), verbose=verbose, weight=weight, maxduration=maxduration, method=method, ...))
    }
    if ( is.editset(E) ){
        B <- separate(E)
    } else {
        B  <- blocks(E)
    }

    n <- length(B)
    i <- 0
    blockCount <- NULL
    err <- checkDatamodel(E,dat,weight)
    # values not in datamodel are set to NA
    dat[err$adapt] <- NA
    for ( b in B ){
        if ( verbose ){
            i <- i + 1
            blockCount <- paste('Processing block',format(i,width=nchar(n)), 'of',n)
        }

        err <- err %+% localize(
            b, 
            dat, 
            verbose, 
            pretext=blockCount, 
            call=sys.call(),
            weight=weight, 
            maxduration=maxduration,
            method, ...)
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
#' @param weight either \code{1xncol(dat)} or \code{nrow(dat)xncol(dat)} array of weights
#' @param method should errorlocalizer ("localizer") or mix integer programming ("mip") be used?
#' @param call call to include in \code{\link{errorLocation}} object
#' @param maxduration max time for searchBest()
#' 
#' @keywords internal
localize <- function(E, dat, verbose, pretext="", call=sys.call(), weight, maxduration, method=c("localizer", "mip"), ...){

    vars <- getVars(E)
    wt <- weight[,vars,drop=FALSE]
    weightperrecord <- nrow(weight) > 1    

    n <- nrow(dat)
    m <- ncol(dat)
    err <- array(FALSE,
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
    wgt <- rep(NA,n)
    degeneracy <- rep(NA,n)
    maxDurationExceeded <- logical(n)
    fmt <- paste('\r%s, record %',nchar(n),'d of %d',sep="")
    method <- match.arg(method)
    if (method == "localizer"){
      for ( i in 1:n ){
          if (verbose){ 
              cat(sprintf(fmt,pretext,i,n)) 
              flush.console() 
          }
          r <- as.list(dat[i,vars,drop=FALSE])
          if (weightperrecord) wt <- weight[i,]
          bt <- errorLocalizer(E, r, weight=wt, ...)
          e <- bt$searchBest(maxduration=maxduration)
          if (!is.null(e) && !bt$maxdurationExceeded){
              err[i,vars] <- e$adapt
              wgt[i] <- e$w
          }
          degeneracy[i] <- bt$degeneracy
          duration[i,] <- getDuration(bt$duration)
          maxDurationExceeded[i] <- bt$maxdurationExceeded
      }
    } else if (method == "mip"){
      for ( i in 1:n ){
        if (verbose){ 
          cat(sprintf(fmt,pretext,i,n)) 
          flush.console() 
        }
        r <- as.list(dat[i,vars,drop=FALSE])
        if (weightperrecord) wt <- weight[i,]
        le <- localize_mip_rec(E, r, weight=wt, ...)
        if (!le$maxdurationExceeded){
          err[i,vars] <- le$adapt
          wgt[i] <- le$w
        }
        degeneracy[i] <- NA
        duration[i,] <- getDuration(le$duration)
        maxDurationExceeded[i] <- le$maxdurationExceeded
      }      
    }
    newerrorlocation(
        adapt=err,
        status = data.frame(
            weight  = wgt,
            degeneracy=degeneracy,
            duration,
            maxDurationExceeded
            ),
        call=call,
        method=method,
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









