#' Check data against a datamodel
#'
#' Variables in \code{dat} which also occur in \code{E} are checked against the datamodel for 
#' those variables. Currently, this function is only usefull for categorical data (i.e. objects of class 
#' \code{\link{editarray}}
#'
#' @param E an object of class \code{\link{editarray}} or \code{\link{editmatrix}}
#' @param dat a \code{data.frame}
#' @param weight vector of weigths for every variable of \code{dat} or an array of weight of the same dimensions as \code{dat}.
#' @param ... arguments to be passed to or from other methods
#'  
#' @return An object of class \code{\link{errorLocation}}
#' @export
checkDatamodel <- function(E, dat,weight=rep(1,ncol(dat)), ...){
    UseMethod('checkDatamodel')
}

#' Check numerical data against datamodel (not implemented) 
#'
#' Currently, this method returns \code{NULL}, and is only used internally.
#'
#' @method checkDatamodel editmatrix
#' @rdname checkDatamodel
checkDatamodel.editmatrix <- function(E, dat, weight=rep(1,ncol(dat)), ...){
    NULL
}

#' Check categorical data against datamodel
#'
#' In an \code{\link{editarray}}, each row codes a multivariate edit, while the columns 
#' define the datamodel. This function checks if every variable, occuring in \code{dat} 
#' takes values in the datamodel of \code{E}. The function is used by \code{\link{localizeErrors}} 
#' prior to performing error localization on multivariate edits. 
#'
#' @method checkDatamodel editarray
#' @rdname checkDatamodel
#'
checkDatamodel.editarray <- function(E, dat, weight=rep(1,ncol(dat)), ...){

    if (any(! (getVars(E) %in% names(dat)) ) ){ 
            vr <- paste(getVars(E)[!getVars(E) %in% names(dat)],collapse=', ')
            stop(paste('Variables',vr,'defined in E not present in dat'))
    }

    m <- nrow(dat)
    if ( is.vector(weight) ){
        weight <- t(array(rep(weight,m),dim=c(ncol(dat),m)))
        dimnames(weight) <- dimnames(dat)
    }
    I <- names(dat)[names(dat) %in% getVars(E)]
    adapt <- array(FALSE,dim=dim(dat),dimnames=dimnames(dat))
    ind <- getInd(E)

    w = rep(0,m)
    for ( ii in I ){
        J <- !(dat[,ii] %in% names(ind[[ii]]))
        adapt[,ii] <- J
        w[J] <- w[J] + weight[J,ii]
    }
    status <- emptyStatus(n=m)
    status$weight <- w
    newerrorlocation(
        adapt  = adapt,
        status = status,
        method = 'checkDatamodel',
    )
}

