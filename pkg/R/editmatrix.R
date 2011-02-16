retrieveSign <- function(e, co=1){
   #stopifnot(is.language(e))
   if (length(e) == 1){
     if (is.numeric(e)){
        l <- co*e
        names(l) <- getOption("editrules.CONSTANT", "CONSTANT")
     }
     else {
        l <- co
        names(l) <- as.character(e)
     }
	  return(l)
   }
   if (length(e) == 2){
     op <- deparse(e[[1]])
	  rhs <- e[[2]]
     if (op == "("){
	    return(retrieveSign(rhs, co))
	  } 
     else if (op == "-"){
        return(retrieveSign(rhs, -1*co))
     }
	  else { 
		stop(". Operator ", op, " not implemented", "Invalid expression:", e)
	  }
   }
   if (length(e) == 3){
     op <- deparse(e[[1]])
	  lhs <- e[[2]]
	  rhs <- e[[3]]
	  lsign <- rsign <- co
     if ( op %in% c("==", "<", "<=", "-") ){
	    rsign <- -1 * co
	  } 
	  else if ( op %in% c(">", ">=") ){
         lsign <- -1 * co
	  }
	  else if (op == "+"){
	  }
	  else if (op == "*"){
       co <- retrieveSign(lhs, co)
       if (!is.numeric(co)){
                stop(paste("Expression contains nonconstant coefficient", paste(lhs,collapse="")))
       }
       return(retrieveSign(rhs, co))
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
#'    \item a \code{data.frame} with three columns:
#'       \itemize{
#'            \item name = a \code{character} with the name of each rule
#'            \item edit = a \code{character} with (in)equalities written in R syntax
#'            \item description = a \code{character} describing the intention of the rule
#'       }
#'      Typically these rules are stored in a external csv file (or database). 
#' }
#'
#' The second form is the prefered form, because it allows the documentation of constraints. This
#' may be very useful when the incorrect observations are analyzed.
#' If the first form is used, \code{editmatrix} internally creates the second form. This information
#' can be retrieved by using \code{\link{editrules}}
#'
#' The matrix is created by retrieving the coefficients of the variables in the equalities.
#' i.e. \code{x == y}   results in  \code{c(x=-1, y=1, w=0, z=0)}
#' and \code{x == y + w} results in \code{c(x=-1, y=1, w=1, z=0)}
#'
#' The edits are canonized: all edits are transformed into an E == C, E < C or E <= C form, so that in the specification of the edit rules all inequalities can be mixed, but the resulting matrix has similar sign.
#' @title Reading in edit rules
#' @seealso \code{\link{editrules}} \code{\link{as.editmatrix}}
#' @export
#' @example examples/editmatrix.R
#'
#' @param editrules \code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
#'        a \code{character} with (in)equalities written in R syntax
#' @param editsinfo deprecated
#'
#' @return an object of class "editmatrix" which is a \code{matrix} with extra attributes
editmatrix <- function( editrules = editsinfo
					       , editsinfo = NULL
					       ){
   if (!missing(editsinfo)){
      warning("this parameter is deprecated, please use parameter editrules")
   }
   
   if (is.character(editrules)){
      edit <- editrules
      name <- NULL
      description <- NULL
      editrules <- NULL
	}
	else if (is.data.frame(editrules)){
      name <- editrules$name
      edit <- editrules$edit
      description <- editrules$description

      if (is.null(edit)){
         stop("The supplied data.frame misses the column 'edit'.\nSee ?editmatrix for a valid input specification")
      }      
      
      editrules$name <- NULL
      editrules$edit <- NULL
      editrules$description <- NULL      
	}
   else {
      stop("Invalid input, please use a character vector or a data.frame.\n See ?editmatrix for a valid input specification")
   }

   #TODO trycatch the parsing...
   edts <- parse(text=edit)
   edit=sapply(edts, deparse)

	if (is.null(name)){
	   name <- paste("e", seq_along(edts),sep="")
	}
	
	if (is.null(description)){
	   description <- rep("", length(edts))
	}
   
	editrules <- as.data.frame(cbind(name,edit,description, editrules))

	stopifnot(is.language(edts))
    
   rowedts <- lapply(edts, function(edt){makeEditRow(edt)})
   
   # normalize operators (the edit matrix is of form E == C or E <= C)
   ops <- sapply( edts
                , function(e){
                    op <- deparse(e[[1]])
                    if (op == ">"){
                       "<="
                    }
                    else if (op == ">="){
                       "<"
                    }
                    else{
                      op
                    }
                  }
                )
	vars <- unique(names(unlist(rowedts)))

	mat <- matrix( 0
	             , ncol=length(vars)
                , nrow=length(rowedts)
                , dimnames = list( rules = editrules$name
                                 , var=vars
                                 )
                )

	for (i in 1:length(rowedts)){
	   mat[i,names(rowedts[[i]])] <- rowedts[[i]]
   }
	structure( mat
	         , class="editmatrix"
            , editrules=editrules
            , edits = edts
            , ops = ops
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

edits <- function(x){
   stopifnot(is.editmatrix(x))
   return(attr(x, "edits"))
}

#' Transform a matrix into an edit matrix
#'
#' The columns of the matrix
#' are the variables and the rows are the edit rules (contraints).
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @param x object to be transformed into an \code{\link{editmatrix}}
#'
#' @return an object of class \code{editmatrix}.
as.editmatrix <- function(x){
   if (is.editmatrix(x)){
      return(x)
   }
   mat <- as.matrix(x)
   structure( mat
            , class="editmatrix"
			   , editrules=editrules(mat)
			   )
}

#' Convert an editmatrix to a normal matrix
#' 
#' An \code{editmatrix} is a matrix and can be used as such, but it has extra attributes.
#' In some case it is preferable to convert the editmatrix to a normal matrix.
#'
#' @export
#' @method as.matrix editmatrix
#'
#' @param x editmatrix object
#' @param ... further arguments passed to or from other methods.
#'
#' @return matrix equal to editmatrix
as.matrix.editmatrix <- function(x, ...){
   array(x, dim=dim(x), dimnames=dimnames(x))
}

#' Convert an editmatrix to a \code{data.frame}
#'
#' Convert an editmatrix to a \code{data.frame}
#' @export 
#' @method as.data.frame editmatrix
#' @param x editmatrix object
#' @param ... further arguments passed to or from other methods.
#'
#' @return data.frame equal to matrix representation of \code{x}
as.data.frame.editmatrix <- function(x, ...){
   as.data.frame(as.matrix(x))
}

#' print edit matrix
#'
#' @export
#' @method print editmatrix
#'
#' @param x editmatrix object to be printed
#' @param ... further arguments passed to or from other methods.
print.editmatrix <- function(x, ...){
   cat("Edit matrix:\n")
   print(as.matrix(x,...))
   cat("\nEdit rules:\n")
   info <- editrules(x)
   desc <- paste("[",info$description,"]")
   desc <- ifelse(info$description=="","", desc)
   cat( paste( info$name,":", info$edit, desc, collapse="\n")
      , "\n"
      )
}
