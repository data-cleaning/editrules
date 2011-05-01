#' Localize errors in numerical data
#'
#' Find the smallest (weighted) set of variables occuring in all violated explicit
#' and implied edits using a branch and bound algoritm.
#'
#' @param E an \code{\link{editmatrix}}
#' @param x a numerical record
#'
#' @nord
localizeErrors <- function(E, x, weight){
    totreat <- getVars(E)
    adapt <- logical(length(totreat))    
    names(adapt) <- totreat

    cp <- choicepoint(
        isSolution = {
            if (length(totreat) == 0){
                wsol <- w
                return(TRUE)
            }
            if (isObviouslyUnfeasable(E) || w > wsol ) return(FALSE)
        },
        choiceLeft = {
            E <- eliminate(E, totreat[1])
            adapt[totreat[1]] <- TRUE
            totreat <- totreat[-1]
            w <- sum(weight[adapt])
        },
        choiceRight = {
            E <- replaceValue(E,x[totreat[1]])
            totreat <- totreat[-1]
        },
        E = E,
        x = x,
        totreat = totreat,
        adapt = adapt,
        w = 0,
        weight = weight,
        wsol = sum(weight)
    ) 
    return(cp)
}

















