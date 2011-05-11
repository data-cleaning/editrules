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
#' @example examples/checkRows.R
#' @export
checkRows <- function(E, dat){
    UseMethod("checkRows")
}

#' @nord
#' @export
checkRows.editmatrix <- function(E, dat){
    stopifnot(is.data.frame(dat))
    vars <- getVars(E) %in% names(dat)
    if (!all(vars)){
       stop("Edits contain variable(s):", paste(colnames(E)[!vars], collapse=","), 
            ", that are not available in the data.frame")
    }
    
    check <- !logical(nrow(dat))
    ed <- as.expression(E)
    for ( i in 1:length(ed)){
        check <- check & tryCatch(eval(ed[[i]], envir=dat), error=function(e){
            stop(paste("Edit",ed[[i]],"can not be checked. Evaluation returned",e$message,sep="\n" ))
        })
    }
    return(check)    
} 

#' @nord
#' @export
checkRows.character <- function(E, dat){
   ed <- parseEdits(E)
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
    checkRows(as.character(E$edit),dat)
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
#' @example examples/violatedEdits.R
#' @export
#' @seealso \code{\link{listViolatedEdits}}, \code{\link{checkRows}}
#' @param E \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked, if a named vector is supplied it will converted internally to a data.frame
#' @param ... further arguments that can be used by methods implementing this generic function
#' @return a logical matrix where each row indicates which contraints are violated
violatedEdits <- function(E, dat, ...){
    UseMethod("violatedEdits")
}


#' @nord
#' @export
violatedEdits.character <- function(E, dat, name=NULL, ...){
    ed <- parseEdits(E)
    if (is.vector(dat) && !is.null(names(dat))){
       dat <- data.frame(t(dat))
    }
    M <- tryCatch(sapply(ed, eval, envir=dat), error=function(e){
        stop(paste("Not all edits can be evaluated, parser returned", e$message, sep="\n"))})
    if ( is.vector(M) )  M <- array(M, dim=c(1,length(M)))
    colnames(M) <- name
    return(!M)
}

#' @nord
#' @export
violatedEdits.editmatrix <- function(E, dat, ...){
# TODO make a real matrix method, add tol argument.
    er <- editrules(E)
    return(violatedEdits.character(er$edit, dat, er$name))
}

#' @nord
#' @export
violatedEdits.data.frame <- function(E, dat, ...){
    if ( !all(c("name","edit","description") %in% names(E)) ){
        stop("Invalid input data.frame see ?editmatrix for valid input format")
    }
    return(violatedEdits.character(as.character(E$edit), dat, E$name))
}


#' Lists which rows of \code{data.frame dat} violate which constraints
#'
#' This function can be used as an input for automatic corrections methods.
#' @example examples/listViolatedEdits.R
#' @export
#' @param E a number of edit restrictions, represented as \code{character} vector, \code{\link{editmatrix}} or \code{data.frame}.
#' @param dat \code{data.frame} with data that should be checked
#' @seealso \code{\link{violatedEdits}} \code{\link{checkRows}}
#' @return a list with per row a \code{integer} vector of the constraints that are violated 
listViolatedEdits <- function(E, dat){    
    errors <- violatedEdits(E, dat)
    errorlist <- apply(errors, 1, which)
    return(apply(errors, 1, which))
}


#' Parse a character vector of edits
#'
#' This function wraps the native \code{\link{parse}} function in a \code{\link{tryCatch}}.
#' The function is \code{editrules} internal. It tries to give a meaningfull error message when
#' parsing fails for some reason.
#'
#' @param E \code{character}
#' @return The edits in \code{E} parsed to R expressions.
#'
parseEdits <- function(E){
     return(
        tryCatch(parse(text=E), 
            error=function(e){
                stop(paste("Not all edits can be parsed, parser returned", e$message,sep="\n"))
            }
        )
    )
}
