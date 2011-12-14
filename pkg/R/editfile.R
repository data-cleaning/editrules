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
#' @param expandEdits should edits be expanded with \code{\link{expandEdits}}?
#' @param ... extra parameters that will be passed to \code{expandEdits}
#'
#' @return \code{\link{editarray}} if \code{type='cat'}, \code{\link{editmatrix}} if \code{type='num'}, \code{list} if \code{type=all}.
#'   If the return value is a \code{list}, the elements are named \code{numedits} and \code{catedits}.
#'
#' @export
editfile <- function(file,type=c("all","num","cat","mix"), expandEdits=FALSE, ...){
    type <- match.arg(type)
    if (!type %in% c('num','cat','all')) stop(paste("type",type,"invalid or not implemented yet"))
    p <- parse(file=file)
    ass <- sapply(p,class) %in% c('<-','=')
    e <- new.env()
    lapply(p[ass],eval,envir=e)
    edits <- p[!ass]
    if (expandEdits){
      l <- c(list(s=edits), as.list(e), list(...))
      edits <- do.call("expandEdits", l)
    }
    numedits <- edits[editTypes(edits) == 'num']
    catedits <- edits[editTypes(edits) == 'cat']
    
    switch(type,
        num = editmatrix(numedits),
        cat = editarray(catedits,env=e),
        all = list(
            catedits=editarray(catedits,env=e),
            numedits=editmatrix(numedits)
        )
    )
        
}

editfile("d:/testeditfile.R", expandEdits=TRUE)