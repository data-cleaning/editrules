#' Localize errors in numerical data
#' 
#' 
#' Returns a \code{\link{choicepoint}} object for error localization in numerical data.
#' The returned choicepoint containts methods to search depth-first to the least weighted
#' number of variables that need to be adapted so that all restrictions in E can be 
#' satisfied. (Generalized principle of Fellegi and Holt (1976)).
#'
#' The search is excecuted with a branch-and-bound algorithm, where in the left branche,
#' a variable is assumed correct and its value subsituted in \code{E}, while in the right
#' branche a variable is assumed incorrect and eliminated from \code{E} with Fourier-Motzkin
#' elimination. See De Waal (2003), chapter 8 for a consice description.
#'
#' Every call to \code{<choicepoint>$searchNext()} returns one solution \code{list}, consisting of
#' \itemize{
#' \item{w: The solution weight.} 
#' \item{adapt: \code{logical} indicating whether a variable should be adapted (\code{TRUE}) or not}
#' \item{E: The \code{\link{editmatrix}} with all variables to adapt eliminated}}
#'
#' Every subsequent call leads either to \code{NULL}, in which case all solutions have been found,
#' or a new solution with a weight \code{w} not higher than the weight of the last found solution.
#'
#' The choicepoint is prepared such that missing data in the input record \code{x} is already
#' set to adapt, and missing variables have been eliminated already.
#'
#' @title Localize errors in numerical data based on the paradigm of Fellegi and Holt.
#'
#' @param E an \code{\link{editmatrix}}
#' @param x a named numerical vecor. The record for which errors will be localized.
#' @param weight a weight vector of length x
#'
#' @return an object of class \code{\link{choicepoint}}. Each execution of \code{$searchNext()} yields a solution
#'      in the form of a \code{list} (see details).
#'
#' @example examples/cpeditmatrix.R
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
cp.editmatrix <- function(E, x, weight=rep(1,length(x))){
    if ( !isNormalized(E) ) E <- normalize(E)
    # missings must be adapted, others still have to be treated.
    adapt <- is.na(x)   
    names(adapt) <- names(x)

    #order decreasing by weight
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]

    # Eliminate missing variables.
    vars <- getVars(E)
    for (v in vars[adapt & names(x) %in% vars]) E <- eliminateFM(E,v)
    wsol <- sum(weight)
    cp <- choicepoint(
        isSolution = {
            w <- sum(weight[adapt])
            if ( isObviouslyInfeasible(E) || w > wsol ) return(FALSE)
            if (length(totreat) == 0){
                wsol <<- w
                adapt <- adapt # neccessary because adapt won't be in the solution if it is not really changed.
                # remove totreat, not necessary in solution
                # other option would be to rename totreat into .totreat, because .names are not exported unless VERBOSE is set to TRUE
                rm(totreat)
                
                return(TRUE)
            }
        },
        choiceLeft = {
            .var <- totreat[1]
            E <- replaceValue(E, .var , x[.var])
            adapt[.var] <- FALSE
            totreat <- totreat[-1]
        },
        choiceRight = {
            .var <- totreat[1]
            E <- eliminateFM(E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        E = E,
        x = x,
        totreat = totreat,
        adapt = adapt,
        weight = weight,
        wsol = wsol 
    )
    #TODO add searchBest
    
    cp
}
