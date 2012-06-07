#' Localize errors on records in a data.frame.
#' 
#' For each record in a \code{data.frame}, the least (weighted) number of fields is
#' determined which can be adapted or imputed so that no edit in \code{E} is violated. Anymore.
#' 
#' For performance purposes, the edits are split in independent \code{\link{blocks}} which are processed
#' separately. Also, a quick vectorized check with \code{\link{checkDatamodel}} is performed first to
#' exclude variables violating their one-dimensional bounds from further calculations. 
#'
#' By default, all weights are set equal to one (each variable is considered equally reliable). If a vector 
#' of weights is passed, the weights are assumed to be in the same order as the columns of \code{dat}. By passing
#' an array of weights (of same dimensions as \code{dat}) separate weights can be specified for each record.
#'
#' In general, the solotion to an error localiztion problem need not be unique, especially when no weights 
#' are defined. In such cases, \code{localizeErrors} chooses a solution randomly. See \code{\link{errorLocalizer}}
#' for more control options.
#'
#' Error localization can be performed by the Branch and Bound method of De Waal (2003) (option \code{method="localizer"}, the default) 
#' or by rewriting the problem as a mixed-integer programming (MIP) problem (\code{method="mip"}) which is parsed to
#' the \code{lpsolve} library. The former case uses \code{\link{errorLocalizer}} and is very reliable in terms
#' of numerical stability, but may be slower in some cases (see note below). The MIP approach is much faster, 
#' but requires that upper and lower bounds are set on each numerical variable. Sensible bounds are derived
#' automatically (see the vignette on error localization as MIP), but could cause instabilities in very rare cases.
#'
#' @note The Branch and Bound method is potentially slow for large sets of connected edits, especially
#'      when conditional edits are involved. Consider using \code{method="mip"} in such cases. The run-time
#'      of the B&B algorithm is related to the number of uquivalent solutions, so setting different weights
#'      (reducing the number of unique solutions) mey reduce computation time as well.
#'
#' @param E an object of class \code{\link{editset}} \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param dat a \code{data.frame} with variables in E.
#' @param useBlocks \code{DEPRECATED}. Process error localization seperatly for independent blocks in \code{E} (always \code{TRUE})?
#' @param verbose print progress to screen?
#' @param weight Vector of positive weights for every variable in \code{dat}, or 
#'      an array of weights with the same dimensions as \code{dat}.
#' @param method should errorlocalizer ("localizer") or mix integer programming ("mip") be used? 
#' @param maxduration maximum time for \code{$searchBest()} to find the best solution for a single record.
#' @param ... Further options to be passed to \code{\link{errorLocalizer}}
#'  
#' @seealso \code{\link{errorLocalizer}}
#' @return an object of class \code{\link{errorLocation}}
#' @example ../examples/localizeErrors.R
#'
#' @references
#'  T. De Waal (2003) Processing of Erroneous and Unsafe Data. PhD thesis, University of Rotterdam.
#'
#'  E. De Jonge and Van der Loo, M. (2012) Error localization as a mixed-integer program in 
#'  editrules (included with the package)
#'
#'  lp_solve and Kjell Konis. (2011). lpSolveAPI: R Interface for
#'  lp_solve version 5.5.2.0. R package version 5.5.2.0-5.
#'  http://CRAN.R-project.org/package=lpSolveAPI
#'
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
    dat <- data.frame(
        rapply(
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
    # check for lpSolveApi 
    if ( match.arg(method) == "mip" ) checklpSolveAPI()

    # separate E in independent blocks
    if ( is.editset(E) && !method=="mip"){
        B <- separate(E)
    } else {
        B  <- blocks(E)
    }
    
    # detect singletons 
    st <- sapply(B,function(b) length(getVars(b)) == 1)
    n <- max(sum(!st),1)
    i <- 0
    err <- checkDatamodel(E,dat,weight)
    # values not in datamodel are set to NA
    dat[err$adapt] <- NA
    for ( b in B[!st] ){
        if ( verbose ){
            i <- i + 1
            blockCount <- paste('Processing block ',format(i,width=nchar(n)), ' of ',n,',',sep="")
        }

        err <- err %+% localize(
            b, 
            dat, 
            verbose, 
            pretext=blockCount, 
            call=sys.call(),
            weight=weight, 
            maxduration=maxduration,
            method=method, ...)
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
localize <- function(E, dat, verbose, pretext="Processing", call=sys.call(), weight, maxduration, method=c("localizer", "mip"), ...){

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
    fmt <- paste('\r%s record %',nchar(n),'d of %d',sep="")
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
      xlim <- generateXlims(dat, ...)
      for ( i in 1:n ){
        if (verbose){ 
          cat(sprintf(fmt,pretext,i,n)) 
          flush.console() 
        }
        r <- as.list(dat[i,vars,drop=FALSE])
        if (weightperrecord) wt <- weight[i,]
        le <- errorLocalizer.mip(E, r, weight=wt, xlim=xlim, ...)
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

# error localization for simple 1-var edits stored as editsets/arrays/matrices/cateditmatrices
#
localize_singleton <- function(E, dat, 
        weight=rep(1,ncol(dat)), 
        method="singleton", 
        call=sys.call(-1), 
        user=Sys.info()["user"], 
        timestamp = date())
{

    if (nedits(E) == 0) return(emptyerrorlocation(dat, method=method, call=call, user=user, timestamp=timestamp ))
    
    n <- nrow(dat)
    if ( is.vector(weight) || nrow(weight)==1 ) weight <- matrix(rep(weight,n), nrow=n ,byrow=TRUE)
    vars <- getVars(E)
    v <- violatedEdits(E,dat)
    v[is.na(v)] <- TRUE
    adapt <- array(FALSE,dim=c(nrow(dat),ncol(dat)),dimnames=list(record=1:n, adapt=names(dat)))
    adapt[,vars] <-  v %*% contains(E) > 0


    # derive status and create errorLocation object.
    status <- data.frame(
        weight = rowSums(adapt*weight),
        degeneracy = rep(1,n),
        user = numeric(n),
        system=numeric(n),
        elapsed=numeric(n),
        maxDurationExceeded=logical(n)
    )
    newerrorlocation(
        adapt       = adapt,
        status      = status, 
        call        = call,
        method      = method,
        user        = user,
        timestamp   = timestamp
    )
}   







