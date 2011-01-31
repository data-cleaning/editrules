retrieveSign <- function(e, fac=1){
   #stopifnot(is.language(e))
   if (length(e) == 1){
      #TODO check for numerics
      l <- fac
	  names(l) <- as.character(e)
	  return(l)
   }
   if (length(e) == 2){
      op <- deparse(e[[1]])
	  rhs <- e[[2]]
      if (op == "("){
	    return(retrieveSign(rhs, fac))
	  } 
   }
   if (length(e) == 3){
      op <- deparse(e[[1]])
	  lhs <- e[[2]]
	  rhs <- e[[3]]
	  lsign <- rsign <- fac
      if ( op == "=="
	    || op == "<"
	    || op == "=<"
	    || op == "-"
		 ){
	    rsign <- -1 * fac
	  } 
	  else if ( op == ">"
	         || op == ">="
		      ){
	    lsign <- -1 * fac
	  }
	  else if (op == "+"){
	  }
	  else if (op == "*"){
	    #TODO check if lhs is a numeric
	    return(retrieveSign(rhs, eval(lhs)*fac))
	  }
	  else { 
		stop(". Operator ", op, " not implemented", "Invalid expression:", e)
	  }
	  return(c( retrieveSign(lhs, lsign)
		      , retrieveSign(rhs, rsign)
		  )
		)
   }
   stop("Invalid expression:", e)
}

makeEditRow <- function(edt){
  if (length(edt) != 3){
     stop(paste("Invalid edit rule:", edt))
  }
  wgt  <- retrieveSign(edt)
  stopifnot(length(wgt)==length(unique(names(wgt))))
  return(wgt)  
}

#' Transforms a list of R (in)equalities into an edit matrix with a factor for each variable
#'
#' There are two forms of creating an editmatrix:
#' \enumerate{ 
#'    \item a \code{character} vector with (in)equalities written in R syntax
#'    \item a \code{data.frame}(in) with three fields:
#'       \itemize{
#'            \item name = a \code{character} with the name of each rule
#'            \item edit = a \code{character} vector with (in)equalities written in R syntax
#'            \item description = a \code{character} desribing the intention of the rule
#'       }
#'      Typically these rules are stored in a external csv file (or database). 
#' }
#'
#' The second form is the prefered form, because it allows the documentation of constraints. This
#' may be very useful when the incorrect observations are analyzed. 
#'
#' The matrix is created by getting the factors of the variables in the equalities.
#' i.e. \code{x == y}   results in  \code{c(x=-1, y=1, w=0, z=0)}
#' and \code{x == y + w} results in \code{c(x=-1, y=1, w=1, z=0)}
#' @title Reading in edit rules
#' @export
#' @example examples/editmatrix.R
#' @param editsinfo \code{data.frame} with (in)equalities written in R syntax, see details for description
#' @param editrules \code{character} vector with (in)equalities written in R syntax
#' @return an object of class "editmatrix" which is a \code{matrix} with extra properties
editmatrix <- function( editsinfo = NULL
					       , editrules = NULL
					       ){
	
	if (is.null(editsinfo)){
	   if (is.null(editrules)){
		 stop("No valid input")
	   }
	   edts <- parse(text=editrules)
	   editsinfo <- data.frame(edit=sapply(edts, deparse))
	}
	else {
	   edts <- parse(text=editsinfo$edit)
	   editsinfo$edit <- sapply(edts, deparse)
	}

	if (is.null(editsinfo$name)){
	   editsinfo$name <- paste("rule", seq(along.with=edts),sep=" ")
	}
	
	if (is.null(editsinfo$description)){
	   editsinfo$description <- rep("", length(edts))
	}
	editsinfo <- editsinfo[c("name","edit","description")]

	stopifnot(is.language(edts))
    
    rowedts <- lapply(edts, function(edt){makeEditRow(edt)})
	vars <- unique(names(unlist(rowedts)))

	mat <- matrix( 0
	             , ncol=length(vars)
				 , nrow=length(rowedts)
				 , dimnames = list( edits = editsinfo$name
				                  , var=vars
								  )
				 )
				 
	for (i in 1:length(rowedts)){
	   mat[i,names(rowedts[[i]])] <- rowedts[[i]]
    }
	structure( mat
	         , class="editmatrix"
			 , editsinfo=editsinfo
			 , edits = edts
			 )
}

#' Check if object is an editmatrix
#' 
#' @seealso \code{\link{editmatrix}}
#' @export
#' @param x object to be checked
#' @return TRUE if \code{x} is an \code{editmatrix}
is.editmatrix <- function(x){
   return(inherits(x, "editmatrix"))
}


#' Retrieve editinfo on an editmatrix
#'
#' If \code{x} is a normal matrix, the matrix will be considered an \code{editmatrix}. The columns of the matrix
#' are the variables and the rows are the edit rules (contraints).
#' @seealso \code{\link{editmatrix}}
#' @export
#' @param x \code{\link{editmatrix}}  or \code{matrix} object
#' @return \code{data.frame} with information on all edit/constraint rules
editsinfo <- function(x){
   if (is.editmatrix(x)){
	return(attr(x, "editsinfo"))
   }
   
   mat <- as.matrix(x)
   if (is.null(mat)){
      stop("x should be a matrix")
   }
   
   vars <- colnames(mat)
   if (is.null(vars)){
       if ((n <- ncol(mat)) > length(letters)){
		   vars <- character(ncol(mat))
		   vars[1:ncol(mat)] <- letters	      
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
   for (i in 1:nrow(mat)){
      r <- mat[i,]
	  lhs <- r > 0
	  rhs <- r < 0
	  
	  r <- abs(r)
	  
	  facs <- paste(r, "*", vars, sep="")
	  facs[r==0] <- "" #remove 0's
	  facs[r==1] <- vars[r==1] #simplify 1's
	  
	  leftterm <- if (any(lhs)) paste(facs[lhs], collapse=' + ')
	              else 0
				  
	  rightterm <- if (any(rhs)) paste(facs[rhs], collapse=' + ')
	               else 0
	  er[i] <- paste(leftterm, "==", rightterm)
   }
   
   data.frame( name=rulenames
             , edit=er
			 , description=""
			 )
}

#' Retrieve parsed R object of edit rules
#' 
#' @param x object of class \code{\link{editmatrix}}
#' @return parsed R object of of the edit rules/contraints
edits <- function(x){
   stopifnot(is.editmatrix(x))
   return(attr(x, "edits"))
}

#' Transform a matrix into an edit matrix
#'
#' The columns of the matrix
#' are the variables and the rows are the edit rules (contraints).
#' @export
#' @seealso \code{\link{editmatrix}}
#' @param x object to be transformed into an \code{\link{editmatrix}}
#' @return an object of class \code{editmatrix}.
as.editmatrix <- function(x){
   if (is.editmatrix(x)){
      return(x)
   }
   mat <- as.matrix(x)
   structure( mat
            , class="editmatrix"
			   , editsinfo=editsinfo(mat)
			   )
}

#' Convert an editmatrix to a normal matrix
#' 
#' An \code{editmatrix} is a matrix and can be used as such, but it has extra attributes.
#' In some case it is preferable to convert the editmatrix to a normal matrix.
#' @export
#' @method as.matrix editmatrix
#' @param x editmatrix object
#' @return matrix equal to editmatrix
#' @param ... further arguments passed to or from other methods.
as.matrix.editmatrix <- function(x, ...){
   array(x, dim=dim(x), dimnames=dimnames(x))
}

#' print edit matrix
#'
#' @export
#' @method print editmatrix
#' @param x editmatrix object to be printed
#' @param ... further arguments passed to or from other methods.
print.editmatrix <- function(x, ...){
   cat("Edit matrix:\n")
   print(as.matrix(x,...))
   cat("\nEdit rules:\n")
   info <- editsinfo(x)
   desc <- paste("[",info$description,"]")
   desc <- ifelse(info$description=="","", desc)
   cat( paste( info$name,":", info$edit, desc, collapse="\n")
      , "\n"
      )
}