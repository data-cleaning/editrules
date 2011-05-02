#' Localize errors in numerical data
#'
#' Returns a \code{\link{choicepoint}} object for error localization in numerical data.
#' The returned choicepoint containts methods to search depth-first to the least weighted
#' number of variables that need to be adapted so that all restrictions in E can be 
#' satisfied.
#'
#' The search is excecuted with a branch-and-bound algorithm, where in the left branche,
#' a variable is assumed correct and it's value subsituted in \code{E} while in the right
#' branche a variable is assumed incorrect and eliminated from \code{E} with Fourier-Motzkin
#' elimination.
#'
#' Every call to \code{<choicepoint>$searchNext()} returns one solution \code{list}, consisting of
#' \itemize{
#' \item{w: The solution weight.} 
#' \item{adapt: \code{logical} indicating whether a variable should be adapted (\code{TRUE}) or not}
#' \item{E: The \code{\link{editmatrix}} with all variables to adapt eliminated}}
#'
#' The choicepoint is prepared such that missing data in the input record is already
#' set to adapt, and missing variables have been eliminated already.
#'
#'
#' @param E an \code{\link{editmatrix}}
#' @param x a numerical record
#' @param weight a weight vector of length x
#'
#' @return an object of class \code{\link{choicepoint}}. Each execution of \code{$searchNext()} yields a solution
#'      in the form of a \code{list} (see details).
#'
#' @example examples/cp.editmatrix.R
#' 
#' @export
cp.editmatrix <- function(E, x, weight){
    if ( !isNormalized(E) ) E <- normalize(E)
    # missings must be adapted, others still have to be treated.
    adapt <- is.na(x)   
    totreat <- getVars(E)[!adapt]
    names(adapt) <- totreat

    # Eliminate missing variables.
    for ( v in getVars(E)[adapt] ) E <- eliminate(E,v)

    choicepoint(
        isSolution = {
            if ( isObviouslyUnfeasable(E) || wt > wsol ) return(FALSE)
            if (length(totreat) == 0){
                wsol <<- wt
                rm(totreat)
                return(TRUE)
            }
        },
        choiceLeft = {
            E <- replaceValue(E,totreat[1] ,x[totreat[1]])
            adapt[totreat[1]] <- FALSE
            totreat <- totreat[-1]
            wt <<- sum(weight[adapt])
            w <- wt
        },
        choiceRight = {
            E <- eliminate(E, totreat[1])
            adapt[totreat[1]] <- TRUE
            totreat <- totreat[-1]
            wt <<- sum(weight[adapt])
            w <- wt
        },
        E = E,
        x = x,
        totreat = totreat,
        adapt = adapt,
        wt = sum(adapt),
        weight = weight,
        wsol = sum(weight)
    ) 
}












