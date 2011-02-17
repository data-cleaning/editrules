#' Check rows of data.frame against edits.
#'
#' This is an S3 generic function for checking rows of a \code{data.frame} against
#' a number of restrictions.
#'
#' @param E Edits, in \code{character}, \code{data.frame} or \code{\link{editmatrix}} representation
#' @param dat The data to check.
#'
#' @export
checkRows <- function(E, dat){
    UseMethod("checkRows")
}
#' Check if rows of a \code{data.frame} are valid according to the rules in \code{edtmatrix}
#'
#' This function can be used to seperate the valid rows/observations from the invalid ones.
#' This method will fail if \code{edtmatrix} contains variables that are not available in \code{dat}
#' @example examples/checkRows.R
#' @export
#' @seealso errorMatrix
#' @param E \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked
#' @return a logical vector with \code{length} equal to \code{nrow(dat)}. If a row is valid is \code{TRUE} otherwise \code{FALSE}
checkRows.editmatrix <- function( E
                     , dat
					      ){
	stopifnot(is.data.frame(dat))
	vars <- colnames(E) %in% names(dat)
	if (!all(vars)){
	   stop("Edits contain variable(s):", paste(colnames(E)[!vars], collapse=","), 
            ", that are not available in the data.frame")
	}
	
	edts <- edits(E)
	check <- rep(TRUE, nrow(dat))	
	for (i in seq(along.with=edts)){
	   check <- check & eval(edts[[i]], envir=dat)
	}
<<<<<<< .mine
    return(check)
=======
   check
>>>>>>> .r32
	#TODO make a matrix an do the computation on the matrix.
} 

#' Check if rows of a \code{data.frame} are valid according to the rules in \code{E}
#'
#' @title Check a vector of verbose edits
#' @param E \code{character} vector with verbose edits on columns of \code{dat}
#' @param dat A \code{data.frame}
#' @return A \code{logical} vector of lentgth \code{nrow(dat)}. \code{TRUE} for every row that 
#'      validates at least one of the rules.
#'
#' @example examples/checkRows.R
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

#' edits in data.frame format.
#'
#' @param E aa
#' @param dat bb
#'
#' @export
checkRows.data.frame <- function(E, dat){
    if ( !all(c("name","edit","description") %in% names(E)) ){
        stop("Invalid input data.frame see ?editMatrix for valid input format")
    }
    E <- as.character(E$edit)
    NextMethod("checkRows")
}


#' Check which rows of \code{data.frame dat} violate which constraints
#'
#' This function can be used as an input for automatic corrections methods.
#' This method will fail if \code{edtmatrix} contains variables that are not available in \code{dat}
#' @example examples/errorMatrix.R
#' @export
#' @seealso listErrors
#' @param edtmatrix \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked
#' @return a logical matrix where each row indicates which contraints are violated
errorMatrix <- function( edtmatrix
                       , dat
                       ){
	stopifnot(is.editmatrix(edtmatrix), is.data.frame(dat))
	vars <- colnames(edtmatrix) %in% names(dat)
	if (!all(vars)){
	   stop("Edits contain variable(s):", paste(colnames(edtmatrix)[!vars], collapse=","), ", that are not available in the data.frame")
	}
	
	edts <- edits(edtmatrix)
	errors <- matrix( FALSE
	                , ncol=length(edts)
				       , nrow=nrow(dat)
				       , dimnames=list(rownames(dat), rownames(edtmatrix))
				       )
	for (i in seq(along.with=edts)){
	   errors[,i] <- !eval(edts[[i]], envir=dat)
	}
   errors
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
