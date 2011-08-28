#' Check which edits are violated by which record.
#' @method violatedEdits editarray
#' @param E an \code{\link{editarray}}
#' @param dat \code{data.frame}
#' @param ... arguments to be passed to or from other methods
#' @export
violatedEdits.editarray <- function(E, dat,...){
    s <- getSep(E)
    for ( v in colnames(dat) ) dat[,v] <- paste(v,dat[,v],sep=s)
    apply(dat,1, function(r) {
        apply(E[,r,drop=FALSE],1,all)
        }
    )
}



#' Localize errors in categorical data
#'
#' @method errorLocalizer editarray
#' @rdname errorLocalizer
#' @export
errorLocalizer.editarray <- function(E, x, weight=rep(1,length(x)), ...){
    adapt <- is.na(x)
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]
    weight <- weight[o[!adapt]]

    vars <- getVars.editarray(E)
    for (v in vars[adapt & names(x) %in% vars]) E <- eliminate.editarray(E,v)
    wsol <- sum(weight)
    ind <- getInd(E)
    bt <- backtracker(
        isSolution = {
            w <- sum(weight[adapt])

            if ( w > wsol )  return(FALSE) 
  
            I <- unique(do.call(c, c(ind[totreat],ind[adapt])))
            if ( length(totreat) > 0 &&  any(rowSums(E[,I,drop=FALSE]) == length(I)) ) return(FALSE)
            
            if ( length(totreat) == 0 ){
                # can eliminated variables be filled in?
                I <- do.call(c,ind[adapt])
                if ( length(I) > 0 && any(rowSums(E[,I,drop=FALSE]) == length(I)) ) return(FALSE)
                # Take care of corner case: check that the record is invalid
                if ( length(I) == 0 &&  any(apply(E[,x,drop=FALSE],1,all)) ) return(FALSE)
                # prepare output
                wsol <<- w
                adapt <- adapt 
                rm(totreat)
                return(TRUE)
            } 
        },
        choiceLeft = {
            .var <- totreat[1]
            E <- substValue.editarray(E, .var , x[.var])
            adapt[.var] <- FALSE
            totreat <- totreat[-1]
        },
        choiceRight = {
            .var <- totreat[1]
            E <- eliminate.editarray(E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        E       = E,
        x       = x,
        totreat = totreat,
        adapt   = adapt,
        weight  = weight,
        wsol    = wsol,
        ind     = ind
    )
    bt
}






