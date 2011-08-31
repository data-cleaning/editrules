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
#' Every subsequent call leads either to \code{NULL}, in which case either all solutions have been found,
#' or \code{maxduration} was exceeded. The property \code{<backtracker>$maxdurationExceeded} indicates if this is
#' the case. Otherwise, a new solution with a weight \code{w} not higher than the weight of the last found solution
#' is returned.
#' 
#' Alternatively \code{<backtracker>$searchBest()} will return the best solution found within \code{maxduration} seconds.
#' If multiple equivalent solutions are found, a random one is returned.
#'
#' The backtracker is prepared such that missing data in the input record \code{x} is already
#' set to adapt, and missing variables have been eliminated already.
#'
#' @title Localize errors in numerical data based on the paradigm of Fellegi and Holt.
#'
#' @param E an \code{\link{editmatrix}} or an \code{\link{editarray}}
#' @param x a named numerical vecor (if E is an editmatrix) or a named character vector (if E is an editarray). 
#'    This is the record for which errors will be localized.
#' @param weight A \code{length(x)} positive weight vector.
#' @param ... Arguments to be passed to other methods (e.g. reliability weights)
#'
#' @return an object of class \code{\link{backtracker}}. Each execution of \code{$searchNext()} yields a solution
#'      in the form of a \code{list} (see details). Executing \code{$searchBest()} returns the lowest-weight solution.
#'      When multiple solotions with the same weight are found, \code{$searchBest()} picks one at random.
#'
#' @example ../examples/errorLocalizer.R
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
errorLocalizer <- function(E, x, weight=rep(1,length(x)), ...){
    UseMethod("errorLocalizer")
}





