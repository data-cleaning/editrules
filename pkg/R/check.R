#' Check if rows of a \code{data.frame} violate contraints
#'
#' @export
#' @seealso errorMatrix
#' This function can be used to seperate the valid rows/observations from the invalid ones.
#' @param edtmatrix \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked
#' @return a logical vector with \code{length} equal to \code{nrow(dat)}. If a row is valid is \code{TRUE} otherwise \code{FALSE}
checkRows <- function( edtmatrix
					 , dat
					 ){
	stopifnot(is.editmatrix(edtmatrix), is.data.frame(dat))
	vars <- colnames(edtmatrix) %in% names(dat)
	if (!all(vars)){
	   stop("Edits contain variable(s):", paste(colnames(edtmatrix)[!vars], collapse=","), ", that are not available in the data.frame")
	}
	
	edts <- edits(edtmatrix)
	check <- rep(TRUE, nrow(dat))	
	for (i in seq(along.with=edts)){
	   check <- check & eval(edts[[i]], envir=dat)
	}
    check
	#TODO make a matrix an do the computation on the matrix.
} 

#' Check which rows of a \code{data.frame} violate which constraints
#'
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
				    , dimnames=list(rownames(dat), edts)
				    )
	for (i in seq(along.with=edts)){
	   errors[,i] <- !eval(edts[[i]], envir=dat)
	}
    errors
}

#' Lists which rows of a \code{data.frame} violate which constraints
#'
#' @export
#' @param edtmatrix \code{\link{editmatrix}} containing the constraints for \code{dat}
#' @param dat \code{data.frame} with data that should be checked
#' @return a list where per row a \code{integer} vector of the constraints that are violated 
listErrors <- function( edtmatrix
                      , dat
					  ){	
	errors <- errorMatrix(edtmatrix, dat)
	edts <- edits(edtmatrix)
	errorlist <- apply(errors, 1, which)
	errorlist
}