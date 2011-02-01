#' Retrieve editrules from an editmatrix
#'
#' \code{editrules} returns a data.frame describing the editrules in editmatrix \code{x}. This data.frame can be used to store the
#' editrules in a readable format, so that the editrules can be maintained and documented.
#'
#' The \code{\link{editmatrix}} function can use the output of \code{editrules} to create an \code{editmatrix}.
#'
#' If \code{x} is a normal matrix, the matrix will be considered an \code{editmatrix}. The columns of the matrix
#' are the variables and the rows are the edit rules (constraints).
#' @aliases editrules editsinfo
#' @seealso \code{\link{editmatrix}}
#' @export editrules editsinfo
#' @param x \code{\link{editmatrix}}  or \code{matrix} object
#' @return \code{data.frame} with information on all edit rules / constraints
editrules <- function(x){
   if (is.editmatrix(x)){
      return(attr(x, "editrules"))
   }
   
   mat <- as.matrix(x)
   if (is.null(mat)){
      stop("x should be a matrix")
   }
   
   vars <- colnames(mat)
   if (is.null(vars)){
       if ((n <- ncol(mat)) > length(letters)){
		   vars <- character(n)
		   vars[1:n] <- letters
	   } else{
	      vars <- letters[1:n]
	   }
   }
   vars <- make.names(vars, unique=TRUE)
   colnames(mat) <- vars

   rulenames <- rownames(mat)
   if (is.null(rulenames)){
      rulenames <- paste("rule", seq(to=nrow(mat)))
   }
   
   er <- character(nrow(mat))
   hasConst <- match(getOption("editrules.CONSTANT", "CONSTANT"), vars, nomatch=0)
   for (i in 1:nrow(mat)){
     r <- mat[i,]
	  lhs <- r > 0
	  rhs <- r < 0
	  
	  r <- abs(r)
	  
	  facs <- paste(r, "*", vars, sep="")

	  facs[r==1] <- vars[r==1] #simplify 1's
     
     if (hasConst > 0){ #the constant column should be handled differently
        facs[hasConst] <- r[hasConst]
     }

	  facs[r==0] <- "" #remove 0's
	  
	  leftterm <- if (any(lhs)) paste(facs[lhs], collapse=' + ')
	              else 0
				  
	  rightterm <- if (any(rhs)) paste(facs[rhs], collapse=' + ')
	               else 0
	  er[i] <- paste(leftterm, "==", rightterm)
   }
   data.frame( name=rulenames
             , edit=er
			    , description=""
             , stringsAsFactors = FALSE
			    )
}

editsinfo <- function(x){
   warning("Deprecated function, please use editrules")
   editrules(x)
}