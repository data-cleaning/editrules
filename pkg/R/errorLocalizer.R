#' Localize errors in a record based on Fellegi and Holt's paradigm
#' 
#' Returns a \code{\link{backtracker}} object for error localization in numerical data.
#' The returned backtracker containts methods to search depth-first to the least weighted
#' number of variables that need to be adapted so that all restrictions in E can be 
#' satisfied. (Generalized principle of Fellegi and Holt (1976)).
#'
#' The search is excecuted with a branch-and-bound algorithm, where in the left branche,
#' a variable is assumed correct and its value subsituted in \code{E}, while in the right
#' branche a variable is assumed incorrect and eliminated from \code{E} with Fourier-Motzkin
#' elimination. See De Waal (2003), chapter 8 for a consice description.
#'
#' Every call to \code{<backtracker>$searchNext()} returns one solution \code{list}, consisting of
#' \itemize{
#' \item{w: The solution weight.} 
#' \item{adapt: \code{logical} indicating whether a variable should be adapted (\code{TRUE}) or not}}
#'
#' Every subsequent call leads either to \code{NULL}, in which case all solutions have been found,
#' or a new solution with a weight \code{w} not higher than the weight of the last found solution.
#' 
#' Alternatively \code{<backtracker>$searchBest()} will return the last solution found directly: 
#' the solution has the lowest weight (but there may be more solutions with equal weight).
#'
#' The backtracker is prepared such that missing data in the input record \code{x} is already
#' set to adapt, and missing variables have been eliminated already.
#'
#' @title Localize errors in numerical data based on the paradigm of Fellegi and Holt.
#'
#' @param E an \code{\link{editmatrix}}
#' @param x a named numerical vecor. The record for which errors will be localized.
#' @param ... Arguments to be passed to other methods (e.g. reliability weights)
#'
#' @return an object of class \code{\link{backtracker}}. Each execution of \code{$searchNext()} yields a solution
#'      in the form of a \code{list} (see details). Executing \code{$searchBest()} returns the lowest-weight solution.
#'      When multiple solotions with the same weight are found, \code{$searchBest()} picks one at random.
#'
#' @example examples/errorLocalizer.R
#'
#' @references 
#' I.P. Fellegi and D. Holt (1976). A systematic approach to automatic edit and imputation. 
#' Journal of the American Statistical Association 71, pp 17-25
#'
#' T. De Waal (2003) Processing of unsave and erroneous data.  PhD thesis, Erasmus Research institute 
#' of management, Erasmus university Rotterdam. 
#' http://www.cbs.nl/nl-NL/menu/methoden/onderzoek-methoden/onderzoeksrapporten/proefschriften/2008-proefschrift-de-waal.htm
#' 
#' @export
errorLocalizer <- function(E, x, ...){
    UseMethod("errorLocalizer")
}


#' Localize errors in numerical data
#'
#' @method errorLocalizer editmatrix
#' 
#' @param E Object of class \code{\link{editmatrix}}
#' @param x Data record, in the form of a named numeric vector.
#' @param weight Weight vector, of the same length of \code{x}
#' @param ... arguments to be passed to other methods.
#' @export
errorLocalizer.editmatrix <- function(E, x, weight=rep(1,length(x)),...){
    if ( !isNormalized(E) ) E <- normalize(E)
    
    # missings must be adapted, others still have to be treated.
    adapt <- is.na(x)   
    names(adapt) <- names(x)

    #order decreasing by weight
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]

    # Eliminate missing variables.
    vars <- getVars(E)
    for (v in names(x)[is.na(x)]) E <- eliminateFM(E,v)
    wsol <- sum(weight)
    cp <- backtracker(
        isSolution = {
            w <- sum(weight[adapt])
            if ( isObviouslyInfeasible(.E) || w > wsol ) return(FALSE)
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
            .E <- eliminateFM(.E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        .E = E,
        x = x,
        totreat = totreat,
        adapt = adapt,
        weight = weight,
        wsol = wsol 
    )
    
    # add a searchBest function, currently returns last solution (which has the lowest weight)
    with(cp,{
        searchBest <- function(..., VERBOSE=FALSE){
            l <- searchAll(...,VERBOSE=VERBOSE)
            if (length(l)>1){ # randomize minimal weight solutions
                ws <- sapply(l,function(s) s$w)
                return(l[[sample(which(ws==min(ws)),1)]])
            } else if (length(l)){
                return(l[[length(l)]])
            }
        }
    })
    cp
}

#' Deprecated error localization function.
#'
#' This function is replaced by S3 generic \code{\link{errorLocalizer}}.
#' 
#'
#' @param E editmatrix
#' @param x record
#' @param ... Arguments to be passed to \code{\link{errorLocalizer}}
#' @export
cp.editmatrix <- function(E,x,...){
 stop("This function is deprecated. Use errorLocalizer in stead")
}


