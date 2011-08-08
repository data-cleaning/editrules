#' Check rows of data.frame against edits.
#'
#' This is an S3 generic function for checking rows of a \code{data.frame} against
#' a number of edit restrictions. The edits can be entered either in \code{character}
#' \code{data.frame} or \code{editmatrix} format.
#'
#' If edits are represented as a \code{character} vector, the entries of \code{E} are parsed
#' and evaluated in the environment of \code{dat}
#'
#' If the edits are  represented in a \code{data.frame}, the \code{data.frame} must have the format
#' described in \code{\link{editmatrix}}. The edits are coerced to a character vector, and passed
#' to \code{checkRows.character}.
#'
#' If the edits are represented by an \code{\link{editmatrix}} (representing linear (in)equalities)
#' verbose edits are extracted and passed on to \code{checkRows.character}
#'
#'
#' @aliases checkRows.character checkRows.data.frame checkRows.editmatrix
#'
#' @param E Edits, in \code{character}, \code{data.frame} or \code{\link{editmatrix}} representation
#' @param dat The data to check.
#' @return a logical vector with \code{length} equal to \code{nrow(dat)}. If a row is violates 
#'      no edit restrictions, \code{TRUE} otherwise \code{FALSE}
#'
#' @seealso violatedEdits
#' @example ../examples/checkRows.R
#' @export
checkRows <- function(E, dat){
    UseMethod("checkRows")
}


#' Retrieve which rows of \code{data.frame dat} violate which constraints
#'
#' This is an S3 generic function for checking rows of a \code{data.frame} against
#' a number of edit restrictions. The edits can be entered either in \code{character}
#' \code{data.frame} or \code{editmatrix} format. The returned value is a logical matrix
#' with dimension (number of records )\eqn{times}(number of edits), indicating which
#' record violates (\code{TRUE}) which edit.
#'
#' This function can be used as an input for automatic corrections methods.
#' This method will fail if \code{E} contains variables that are not available in \code{dat}
#' 
#' @aliases violatedEdits.character violatedEdits.data.frame violatedEdits.editmatrix
#' @example ../examples/violatedEdits.R
#' @export
#' @seealso \code{\link{listViolatedEdits}}, \code{\link{checkRows}}
#' @param E \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked, if a named vector is supplied it will converted internally to a data.frame
#' @param ... further arguments that can be used by methods implementing this generic function
#' @return a logical matrix where each row indicates which contraints are violated
violatedEdits <- function(E, dat, ...){
    UseMethod("violatedEdits")
}


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
#' @param E an \code{\link{editmatrix}}
#' @param x a named numerical vecor. The record for which errors will be localized.
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
errorLocalizer <- function(E, x, ...){
    UseMethod("errorLocalizer")
}



#' Find obvious redundancies in set of edits
#'
#' The function returns a logical vector which is TRUE at any row of the system 
#' Ax <operators> b which is obviously redundant. Obvious redundancies, amounting
#' to statements as 0==0 or 0 < 1 may arise durining elimination processes. The 
#' function also checks for duplicate rows in the augmented matrix [A|b]
#' 
#' Extra paramters:
#' \itemize{
#' \item{tol: tolerance to check for zeros, default square root of machine accuracy}
#' \item{duplicates: logical, check for duplicate rows?, default \code{TRUE}}
#' \item{duplicates.tol: tolerance for duplicate search, standard: \code{tol}}
#' }
#' @param E Augmented matrix A|b, editmatrix 
#' @param ... parameters to be passed to or from other methods. 
#' 
#'
#'
#' @seealso \code{\link{isObviouslyRedundant.matrix}}, \code{\link{isObviouslyRedundant.editmatrix}}
#' @export 
isObviouslyRedundant <- function(E, ...){
    UseMethod("isObviouslyRedundant")
}

#' Bring an (edit) matrix to reduced row echelon form.
#'
#' If E is a matrix, a matrix in reduced row echelon form is returned.
#' If E is an \code{\link{editmatrix}} the equality part of E is transformed
#' to reduced row echelon form.
#'
#' @aliases echelon.editmatrix echelon.matrix
#'
#' @param E a matrix or editmatrix
#' @param ... options to pass on to further methods.
#' @export
echelon <- function(E,...){
    UseMethod("echelon")
}

#' eliminate a variable from a set of edit rules
#' 
#' 
#'
#'
#' @param E editmatrix 
#' @param var name of variable to be eliminated
#' @param ... argumemts to be passed to or from other methods
#' @export
eliminate <- function(E, var, ...){
    UseMethod("eliminate")
}



