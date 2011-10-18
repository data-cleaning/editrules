
#' Summarize
#' 
#' Summarizes an object
#'
#' The return value is objec of class 'editsummary', 'array' for which a print method is implemented.
#'
#' @method summary editmatrix
#' @param object an R object
#' @param useBlocks \code{logical} Summarize each block?
#' @param ... Arguments to pass to or from other methods 
#'
#' @rdname summary
#' @export
#' @example ../examples/summary.R
summary.editmatrix <- function(object, useBlocks=TRUE, ...){
    if (useBlocks){ 
        B <- blocks(object)
    } else {
        B <- list(object)
    }
    A <- array(c(
        sapply(B,nrow),
        sapply(B,function(b) sum(getOps(b) == '==')),
        sapply(B,function(b) sum(getOps(b) != '==')),
        sapply(B,function(b) length(getVars(b)) )
        ), 
        dim=c(length(B),4),
        dimnames=list(
            block = 1:length(B),
            count = c('edits','equalities','inequalities','variables')
        )
    )
    structure(A,
        class=c('editsummary','array'),
        type ='editmatrix',
        normalized = isNormalized(object))
}


#' Summary of edit set
#'
#' @method summary editmatrix
#' 
#' @rdname summary
#' @export
#' @example ../examples/summary.R
summary.editarray <- function(object, useBlocks=TRUE, ...){
    if ( useBlocks ){
        B <- blocks(object)
    } else {
        B <- list(object)
    }
    A <- array(c(
        sapply(B,nrow),
        sapply(B,function(b) length(getVars(b)))
        ),
        dim=c(length(B),2),
        dimnames=list(
            block=1:length(B),
            count=c('edits','variables')
        )
    )
    structure(A,
        class='editsummary',
        type ='editarray')
}

print.editsummary <- function(x,...){
    nrm <- ''
    if ( attr(x,'type')=='editmatrix' ){
        nrm <- 'normalized'
        if (!attr(x,'normalized')) nrm = paste('non-',nrm,sep='')
    }
    cat('Summary of',nrm,attr(x,'type'),'\n')
    print(x[,,drop=FALSE])

}




