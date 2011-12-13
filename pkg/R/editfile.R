# parse categorical edits from file

#' Read categorical edits from textfile
#'
#' This utility function allows for a more free format for editrule definition in a file.
#' The function first parses all assignments. Every other statement is considered to
#' be a categorical edit. 
#'
#'
#' @param file name of text file to read in
#' @param type type of edits to extract. Currently, only 'num' (numerical), 'cat' (categorical)  and 'all' are implemented.
#' @param forceCat force mixed edits (e.g. \code{if (a == 1) y \%in\% c('a','b')}) to be processed as fully categorical 
#'      (i.e. the \code{1} will become \code{'1'}). Will crash if edits contain arithmetic operations.
#'
#' @return \code{\link{editarray}} if \code{type='cat'}, \code{\link{editmatrix}} if \code{type='num'}, \code{list} if \code{type=all}.
#'   If the return value is a \code{list}, the elements are named \code{numedits} and \code{catedits}.
#'
#' @export
editfile <- function(file,type=c("all","num","cat","mix"), forceCat=FALSE){
    type <- match.arg(type)
    if (!type %in% c('num','cat','all')) stop(paste("type",type,"invalid or not implemented yet"))
    p <- parse(file=file)
    ass <- sapply(p,class) %in% c('<-','=')
    e <- new.env()
    lapply(p[ass],eval,envir=e)
    edits <- p[!ass]
    numedits <- edits[editTypes(edits) == 'num']
    cats <- ifelse(forceCat,c('cat','mix'),'cat')
    catedits <- edits[editTypes(edits) %in% cats]
    
    switch(type,
        num = editmatrix(numedits),
        cat = editarray(catedits,env=e),
        all = list(
            catedits=editarray(catedits,env=e),
            numedits=editmatrix(numedits)
        )
    )
        
}



