COPS <- c("==","<","<=",">",">=")

retrieveCoef <- function(e, co=1){
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
	    return(retrieveCoef(rhs, co))
	  } 
     else if (op == "-"){
        return(retrieveCoef(rhs, -1*co))
     }
	  else { 
		stop("Operator ", op, " not implemented", "Invalid expression:", e)
	  }
   }
   if (length(e) == 3){
     op <- deparse(e[[1]])
      lhs <- e[[2]]
      rhs <- e[[3]]
      lsign <- rsign <- co
     if ( op %in% c(COPS, "-")){
	    rsign <- -1 * co
	  } 
	  else if (op == "+"){
	  }
	  else if (op == "*"){
       co <- retrieveCoef(lhs, co)
       if (!is.numeric(co)){
                stop(paste("Expression contains nonconstant coefficient", paste(lhs,collapse="")))
       }
       return(retrieveCoef(rhs, co))
	  }
	  else { 
		stop("Operator ", op, " not implemented", "Invalid expression:", e)
	  }
	  return(c( retrieveCoef(lhs, lsign)
		      , retrieveCoef(rhs, rsign)
		  )
		)
   }
   stop("Invalid expression:", e)
}

parseGuard <- function(g){
  op <- as.character(g[[1]])
  if (op %in% c( COPS
               , "||"
               , "&&"
               ,"%in%"
               )
     ){
  }
  else {
     stop("Invalid condition syntax: ", g)
  }
}

makeEditRow <- function(edt){
  op <- as.character(edt[[1]])
  if (op == "if"){
     stop("Conditional edit rules are not (yet) supported.", edt)
     guard <- edt[[2]]
     parseGuard(guard)
     edt <- edt[[3]]
     op <- as.character(edt[[1]])  
  }
  if (!(op %in% COPS)){
     stop(paste("Invalid edit rule:", edt))
  }
  wgt <- retrieveCoef(edt)
  # simplify the coefficients by summing them
  wgt <- tapply(wgt, names(wgt), sum)
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
#' i.e. \code{x == y}   results in  \code{c(x=1, y=-1)}
#' and \code{x == y + w} results in \code{c(x=1, y=-1, w=-1)}
#'
#' By default the editmatrix is created using the comparison operators (\code{==,<=,>=,<,>}) in the edits. If option \code{normalize=TRUE} is used all 
#' edits are transformed into an E == C, E < C or E <= C form, so that in the specification of the edit rules all inequalities can be mixed, 
#' but the resulting matrix has similar sign.
#' @title Reading in edit rules
#' @seealso \code{\link{editrules}} \code{\link{as.editmatrix}}
#' @export
#' @example examples/editmatrix.R
#'
#' @param editrules \code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
#'        a \code{character} with (in)equalities written in R syntax
#' @param normalize \code{logical} specifying if all edits should be transformed (see description)
#'
#' @return an object of class "editmatrix" which is a \code{matrix} with extra attributes
editmatrix <- function( editrules
                      , normalize = FALSE
					       ){   
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

   edts <- tryCatch(parse(text=edit), error=function(e){
         stop(paste("The edits could not be parsed. Parser returned\n",e$message))})   
   stopifnot(is.language(edts))
   
   edit <- sapply(edts, deparse)
   edit <- gsub(" * ","*", fixed=TRUE, edit)
   
	if (is.null(name)){
	   name <- paste("e", seq_along(edit),sep="")
	}
	
	if (is.null(description)){
	   description <- rep("", length(edit))
	}
   
   # create/update the name, edit, description vectors and keep the original other vectors
   editrules <- data.frame( name=name,edit=edit,description=description
                          , stringsAsFactors=FALSE
                          )
    
   rowedts <- lapply(edts, function(edt){makeEditRow(edt)})
   ops <- sapply(edts, function(e){deparse(e[[1]])})
   C <- numeric(length(ops))
   
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
   
   if (normalize){ 
      for (i in 1:nrow(mat)){
         if (ops[i] == ">="){
            mat[i,] <- -mat[i,]
            ops[i] <- "<"
         }
         else if (ops[i] == ">"){
            mat[i,] <- -mat[i,]
            ops[i] <- "<="
         }
      }
   }

   if ((m  <- match("CONSTANT", colnames(mat), nomatch=0))){
      C <- -1*mat[,m]
      mat <- mat[,-m, drop=FALSE]
   }
   
   if (normalize){
      return(as.editmatrix(mat,C=C, ops=ops, normalize=FALSE))
   }

	names(ops) <- name
   names(C) <- name
   
   structure( mat
	         , class="editmatrix"
            , editrules=editrules
            , edits = edts
            , ops = ops
            , C = C
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
#' @param x object to be transformed into an \code{\link{editmatrix}}. \code{x} will be coerced to a matrix.
#' @param C Constant, a \code{numeric} of \code{length(nrow(x))}, defaults to 0
#' @param ops Operators, \code{character} of \code{length(nrow(x))} with the equality operators, defaults to "=="
#' @param ... further parameters will be given to \code{editmatrix}
#'
#' @return an object of class \code{editmatrix}.
as.editmatrix <- function( x
                         , C = numeric(nrow(mat))
                         , ops = rep("==", nrow(mat))
                         , ...
                         ){
   if (is.editmatrix(x)){
      return(x)
   }
   mat <- as.matrix(x)
  
   vars <- colnames(mat)
   if (is.null(vars)){
       if ((n <- ncol(mat)) > length(letters)){
		   vars <- character(n)
		   vars[1:n] <- letters
	   } else{
	      vars <- letters[1:n]
	   }
   }
   colnames(mat) <- make.names(vars, unique=TRUE)
   
   nC <- ncol(mat) + 1
   er <- character(nrow(mat))
   for (i in 1:nrow(mat)){
     r <- c(mat[i,], -C[i])
	  lhs <- r > 0
	  rhs <- r < 0
     
	  r <- abs(r)
	  
	  facs <- paste(r, "*", vars, sep="")
     
	  facs[r==1] <- vars[r==1] #simplify 1's
     
	  facs[r==0] <- "" #remove 0's
	  
     #replace constant term
     facs[nC] <- C[i]
     
	  leftterm <- if (any(lhs)) paste(facs[lhs], collapse=' + ')
	              else 0
				  
	  rightterm <- if (any(rhs)) paste(facs[rhs], collapse=' + ')
	               else 0
	  er[i] <- paste(leftterm, ops[i], rightterm)
   }
   ei <- data.frame(edit=er)
   ei$edit <- er
   ei$name <- rownames(er)
   editmatrix(er,...)
}

#' Convert an editmatrix to a normal matrix
#' 
#' An \code{editmatrix} is a matrix and can be used as such, but it has extra attributes.
#' In some cases it is preferable to convert the editmatrix to a normal matrix.
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
   m <- as.matrix(x)
   m <- cbind(m, CONSTANT=getC(x))
   print(m, ...)
   cat("\nEdit rules:\n")
   info <- editrules(x)
   desc <- paste("[",info$description,"]")
   desc <- ifelse(info$description=="","", desc)
   cat( paste( info$name,":", info$edit, desc, collapse="\n")
      , "\n"
      )
}
