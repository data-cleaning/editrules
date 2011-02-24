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
#' @seealso errorMatrix
#' @example examples/checkRows.R
#' @export
checkRows <- function(E, dat){
    UseMethod("checkRows")
}

#' @nord
#' @export
checkRows.editmatrix <- function(E, dat){
    stopifnot(is.data.frame(dat))
    vars <- colnames(E) %in% names(dat)
    if (!all(vars)){
       stop("Edits contain variable(s):", paste(colnames(E)[!vars], collapse=","), 
            ", that are not available in the data.frame")
    }

    return(checkRows.character(editrules(E)$edit, dat))
    #TODO make a matrix an do the computation on the matrix.
} 

#' @nord
#' @export
checkRows.character <- function(E, dat){
    
    ed <- tryCatch(parse(text=E), error=function(e){
        stop(paste("Not all edits can be parsed, parser returns", e$message,sep="\n"))})
    check <- !logical(nrow(dat))
    for ( i in 1:length(E)){
        check <- check & tryCatch(eval(ed[[i]], envir=dat), error=function(e){
            stop(paste("Edit",ed[[i]],"can not be checked. Evaluation returned",e$message,sep="\n" ))
        })
    }
    return(check)
}

#' @nord
#' @export
checkRows.data.frame <- function(E, dat){
    if ( !all(c("name","edit","description") %in% names(E)) ){
        stop("Invalid input data.frame see ?editMatrix for valid input format")
    }
    return(checkRows.character(as.character(E$edit), dat))
}


#' Check which rows of \code{data.frame dat} violate which constraints
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
#' @aliases errorMatrix.character errorMatrix.data.frame errorMatrix.editmatrix
#' @example examples/errorMatrix.R
#' @export
#' @seealso \code{\link{listErrors}}, \code{\link{checkRows}}
#' @param E \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked
#' @return a logical matrix where each row indicates which contraints are violated
errorMatrix <- function(E, dat){
    UseMethod("errorMatrix")
}


#' @nord
#' @export
errorMatrix.character <- function(E, dat){
    ed <- tryCatch(parse(text=E), error=function(e){
        stop(paste("Not all edits can be parsed, parser returned", e$message,sep="\n"))})
    M <- tryCatch(sapply(ed, eval, envir=dat), error=function(e){
        stop(paste("Not all edits can be evaluated, parser returned", e$message, sep="\n"))})
    return(M)
}

#' @nord
#' @export
errorMatrix.editmatrix <- function(E, dat){
    return(errorMatrix.character(editrules(E)$edit, dat))
}

#' @nord
#' @export
errorMatrix.data.frame <- function(E, dat){
    if ( !all(c("name","edit","description") %in% names(E)) ){
        stop("Invalid input data.frame see ?editMatrix for valid input format")
    }
    return(errorMatrix.character(as.character(E$edit), dat))
}


#' Lists which rows of \code{data.frame dat} violate which constraints
#'
#' This function can be used as an input for automatic corrections methods.
#' @example examples/listErrors.R
#' @seealso errorMatrix
#' @export
#' @param edtmatrix \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked
#' @return a list with per row a \code{integer} vector of the constraints that are violated 
listErrors <- function( edtmatrix
                      , dat
                      ){    
    errors <- errorMatrix(edtmatrix, dat)
    edts <- edits(edtmatrix)
    errorlist <- apply(errors, 1, which)
    errorlist
}
