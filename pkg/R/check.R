#' Check if rows of a data.frame dat pass the editmatrix 
#' @return a logical vector if rows passed all constraints is TRUE otherwise FALSE
checkRows <- function( edtmatrix
					 , dat
					 ){
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

#' Check if rows of a data.frame dat pass the editmatrix 
#' @return a logical matrix with each data row has an error in what edit rule
errorMatrix <- function( edtmatrix
                       , dat
					   ){
	
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

#' Check if rows of a data.frame dat pass the editmatrix 
#' @return a logical vector if rows passed all constraints is TRUE otherwise FALSE
listErrors <- function( edtmatrix
                      , dat
					  ){
	
	errors <- errorMatrix(edtmatrix, dat)
	edts <- edits(edtmatrix)
	errorlist <- apply(errors, 1, which)
	errorlist
}