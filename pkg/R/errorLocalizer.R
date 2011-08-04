
#' Localize errors in numerical data
#'
#' @method errorLocalizer editmatrix
#' 
#' @param E Object of class \code{\link{editmatrix}}
#' @param x Data record, in the form of a named numeric vector.
#' @param weight Weight vector, of the same length of \code{x}
#' @param maxadapt maximum number of variables to adapt
#' @param maxweight maximum weight of solution, if weights are not given, this is equal to the 
#' maximum number of variables to adapt. 
#' @param maxduration maximum time (in seconds), for \code{$searchNext()}, \code{$searchAll()} and \code{$searchBest()} 
#' @param ... arguments to be passed to other methods.
#'
#' @rdname errorLocalizer
#' @export
errorLocalizer.editmatrix <- function(
            E, 
            x, 
            weight=rep(1,length(x)), 
            maxadapt=length(x), 
            maxweight=sum(weight),
            maxduration=600,
            ...){
    # search space larger then 1M:

    if ( !isNormalized(E) ) E <- normalize(E)
    # missings must be adapted, others still have to be treated.
    adapt <- is.na(x)   
    vars <- getVars(E)

    #order decreasing by weight
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]
    # only treat variables in occuring in editmatrix.
    totreat <- totreat[totreat %in% vars]
    # if variables do not occur in editmatrix, do not adapt.
    adapt[!(names(adapt) %in% vars)] <- FALSE
    # Eliminate missing variables.
    for (v in names(x)[is.na(x)]) E <- eliminate.editmatrix(E,v)
    wsol <- min(sum(weight), maxweight)
    cp <- backtracker(
        maxduration=maxduration,
        isSolution = {
            w <- sum(weight[adapt])
            if ( w > wsol 
              || sum(adapt) > maxadapt
              || isObviouslyInfeasible(.E)
               ) return(FALSE)

            if ( w == wsol && isObviouslyInfeasible(substValue(.E,totreat,x[totreat])) ) 
                    return(FALSE)
            # TODO report status
            if (length(totreat) == 0){
                wsol <<- w
                adapt <- adapt 
                rm(totreat)
                return(TRUE)
            }
        },
        choiceLeft = {
            .var <- totreat[1]
            .E <- substValue(.E, .var , x[.var])
            adapt[.var] <- FALSE
            totreat <- totreat[-1]
        },
        choiceRight = {
            .var <- totreat[1]
            .E <- eliminate.editmatrix(.E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        .E = E,
        x = x,
        maxadapt=maxadapt,
        totreat = totreat,
        adapt = adapt,
        weight = weight,
        wsol = wsol
    )
    
    # add a searchBest function, currently returns last solution (which has the lowest weight)
    with(cp,{
        searchBest <- function(maxduration=600, VERBOSE=FALSE){
            l <- searchAll(maxduration=maxduration,VERBOSE=VERBOSE)
            if (length(l)>0){ 
                ws <- sapply(l,function(s) s$w)
                iwmin <- which(ws==min(ws))
                if (length(iwmin) == 1) return(l[[iwmin]])
                return(l[[sample(iwmin,1)]])
            }
        }
    })
    cp
}


