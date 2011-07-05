require(editrules)
# simple edit manipulation with categorical edits.
# m, 04.05.2011

violatedEdits.editarray <- function(E,x){
    if ( is.data.frame(x)) x <- sapply(x,as.character)
    apply(x,1, function(r) 
        apply(E[,r],1,all)
    )
}



# choicepoint object for error localization in categorical data.
# E: editarray, x: character vector.
# This function will become obsolete if the workhorse functions are overloaded.
# TODO: deceide at which level the overloading / specialization limit lies.
#
errorLocalizer.editarray <- function(E, x, weight=rep(1,length(x)), ...){
    adapt <- is.na(x)
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]
    weight <- weight[o[!adapt]]

    vars <- getVars.editarray(E)
    for (v in vars[adapt & names(x) %in% vars]) E <- eliminateFM.editarray(E,v)
    wsol <- sum(weight)
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
            E <- eliminateFM.editarray(E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        E       = E,
        x       = x,
        totreat = totreat,
        adapt   = adapt,
        weight  = weight,
        wsol    = wsol,
        ind     = getInd(E)
    )
    bt
}


















