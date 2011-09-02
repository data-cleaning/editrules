#' Transforms a list of R (in)equalities into an edit matrix.
#'
#' Transforms a list of R (in)equalities into an edit matrix with coefficients (\code{A}) for each variable, and a constant (\code{b})
#' and operator (\code{ops}) for each edit rule.
#'
#' Each row in the resulting editmatrix represents an linear (in) equality.
#' Each column in the resulting editmatrix represents a variable.
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
#' The function \code{\link{editrules}} creates/extracts the second form, which can be used to store edit rules
#' externally or to recreate an editmatrix later on.
#'
#' The matrix is created by retrieving the coefficients of the variables in the equalities.
#' i.e. \code{x == y}   results in  \code{c(x=1, y=-1)}
#' and \code{x == y + w} results in \code{c(x=1, y=-1, w=-1)}
#'
#' By default the editmatrix is created using the comparison operators (\code{==,<=,>=,<,>}) in the edits. If option \code{normalize=TRUE} is used all 
#' edits are transformed into an A == b, A < b or A <= b form, so that in the specification of the edit rules all inequalities can be mixed, 
#' but the resulting matrix has similar sign.
#' @title Create an editmatrix
#' @seealso \code{\link{editrules}} \code{\link{as.editmatrix}}
#' @export
#' @example ../examples/editmatrix.R
#'
#' @param editrules \code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
#'        a \code{character} with (in)equalities written in R syntax
#' @param normalize \code{logical} specifying if all edits should be transformed (see description)
#'
#' @return an object of class "editmatrix" which is a \code{matrix} with extra attributes
editmatrix <- function( editrules
                      , normalize = TRUE
					       ){   
   if (is.character(editrules)){
      edit <- editrules
      name <- NULL
      description <- NULL
      editrules <- NULL
    }
    else if (is.data.frame(editrules)){
      if (is.null(editrules$edit)){
         stop("The supplied data.frame misses the column 'edit'.\nSee ?editmatrix for a valid input specification")
      }            
      name <- editrules$name
      edit <- as.character(editrules$edit)
      description <- editrules$description
     }            
    
   else {
      stop("Invalid input, please use a character vector or a data.frame.\n See ?editmatrix for a valid input specification")
   }

    edts <- parseEdits(edit, type="num")   
  	if (is.null(name)){
  	   name <- paste("e", seq_along(edts),sep="")
  	}
   
    rowedts <- lapply(edts, function(edt){parseNum(edt)})
    ops <- sapply(edts, function(e){deparse(e[[1]])})
   
    vars <- unique(names(unlist(rowedts)))
    vars <- c(vars[vars!="CONSTANT"], "CONSTANT")

    A <- matrix( 0
                 , ncol=length(vars)
                 , nrow=length(rowedts)
                 , dimnames = list( rules = name
                                  , var=vars
                                  )
                 )
                 
    for (i in 1:length(rowedts)){
       A[i,names(rowedts[[i]])] <- rowedts[[i]]
    }
    A[,ncol(A)] <- -A[,ncol(A)]
   
   if (normalize){
      geq <- ops == ">="
      gt <- ops == ">"
      A[geq | gt,] <- -A[geq | gt,]
      ops[geq] <- "<="
      ops[gt] <- "<"      
   }
   
   names(ops) <- name
   E <- neweditmatrix(A, ops=ops, normalized=all(ops %in% c("==","<","<=")))
   attr(E, "description") <- description
   E
}

#' Create an \code{editmatrix} object from its constituing attributes. 
#'
#' This function is for internal purposes, please use \code{\link{editmatrix}} for creating an editmatrix object.
#' @param A An augmented \code{matrix} of the form \code{A|b}
#' @param ops a character vector with the comparison operator of every edit.
#' @param normalized \code{logical} TRUE or FALSE
#' @param ... optional attributes
#' @return an S3 object of class \code{editmatrix} 
neweditmatrix <- function(A, ops, normalized=FALSE,...){
   structure( A
            , class="editmatrix"
            , ops = ops
            , normalized = normalized
            , ...
            )
}





#' Coerce a matrix to an edit matrix.
#'
#' \code{as.editmatrix} interpretes the matrix as an editmatrix.
#' The columns of the matrix
#' are the variables and the rows are the edit rules (contraints).
#' 
#' If only argument \code{x} is given (the default), the resulting editmatrix is of the form \eqn{Ax=0}. 
#' This can be influenced by using the parameters \code{b} and \code{ops}.
#'
#' @export
#' @seealso \code{\link{editmatrix}}
#'
#' @param A matrix to be transformed into an \code{\link{editmatrix}}. 
#' @param b Constant, a \code{numeric} of \code{length(nrow(x))}, defaults to 0
#' @param ops Operators, \code{character} of \code{length(nrow(x))} with the equality operators, defaults to "=="
#' @param ... further attributes that will be attached to the resulting editmatrix
#'
#' @return an object of class \code{editmatrix}.
as.editmatrix <- function( A
                         , b = numeric(nrow(A))
                         , ops = rep("==", nrow(A))
                         , ...
                         ){
    if (is.editmatrix(A)){
        return(A)
    } 
    if (!is.matrix(A)){
        stop("Argument A must be an object of class matrix. ")
    }   
    cn <- colnames(A)
    if (is.null(cn)){
       cn <- make.names(paste("x", 1:ncol(A), sep=""), unique=TRUE)
    }
    rn <- rownames(A)
    if ( is.null(rn) || length(unique(rn)) != length(rn) ){
       rn <- paste("e", 1:nrow(A), sep="")
    }
    A <- cbind(as.matrix(A), b)
    dimnames(A) <- list(rules=rn,var=c(cn,"CONSTANT"))
    E <- neweditmatrix(A=A, ops=ops, ...)
    if (isNormalized(E)) attr(E,"normalized") <- TRUE
    E
}

#' Coerce an editmatrix to a normal matrix
#' 
#' An \code{editmatrix} is a matrix and can be used as such, but it has extra attributes.
#' In some cases it is preferable to convert the editmatrix to a normal matrix.
#
#' Please note that coercion returns the augmented matrix \code{A|b} and not the \code{ops} part.
#'
#' @export
#' @method as.matrix editmatrix
#'
#' @param x editmatrix object
#' @param ... further arguments passed to or from other methods.
#'
#' @return augmented matrix of editmatrix
as.matrix.editmatrix <- function(x, ...){
   array(x, dim=dim(x), dimnames=dimnames(x))
}

#' Coerce an editmatrix to a \code{data.frame}
#'
#' Coerces an editmatrix to a \code{data.frame}. Useful for storing/viewing the matrix representation of editrules.
#' @export 
#' @method as.data.frame editmatrix
#' @param x editmatrix object
#' @param ... further arguments passed to or from other methods.
#'
#' @return data.frame with the coefficient matrix representation of \code{x}, an operator column and CONSTANT column.
as.data.frame.editmatrix <- function(x, ...){
   dat <- as.data.frame(getA(x))
   nms <- make.names( c(names(dat), "Ops", "CONSTANT")
                    , unique=TRUE
                    )
   n <- length(nms)
   dat[[nms[n-1]]] <- getOps(x)
   dat[[nms[n]]] <- getb(x)
   dat
}


#' Coerce an editmatrix to a \code{character} vector
#'
#' Derives readable editrules from an editmatrix.
#' @export
#' @method as.character editmatrix
#'
#' @param x editmatrix object to be printed
#' @param ... further arguments passed to or from other methods.
as.character.editmatrix <- function(x, ...){
   A <- getA(x)
   b <- getb(x)
   vars <- getVars(x)
   ops <- getOps(x)

   n <- ncol(A)

   nC <- ncol(A) + 1
   er <- character(nrow(A))

   left <- right <- character(nrow(A)) 
   for ( i in seq_along(rownames(A)) ){
     r <- A[i,]
     lhs <- r > 0
     rhs <- r < 0
     left[i] <- if(any(lhs)) { paste(r[lhs], "*", vars[lhs],sep="",collapse=" + ") } else ""
     right[i] <-if(any(rhs))  { paste(-r[rhs], "*",vars[rhs],sep="",collapse=" + ") } else ""
   }
   left <- gsub(" 1\\*"," ",left)
   right <- gsub(" 1\\*"," ",right)
   left <- gsub("^1\\*","",left)
   right <- gsub("^1\\*","",right)

   
   left <- ifelse( left==""
                 , ifelse(right=="", "0", -b) 
                 , ifelse(b < 0 & right != "", paste(left,-b,sep=" + "), left)
                 )
   
   right <- ifelse( right==""
                  , b
                  , ifelse(b > 0 & left != -b, paste(right,b,sep=" + "), right)
                  )
   txt <- paste(left,ops,right)    
   names(txt) <- rownames(x)
   txt
}

#' Coerce an editmatrix to R expressions
#'
#' Generates an R \code{expression} vector that can be used to check data using \code{\link{eval}}.
#' @export
#' @method as.expression editmatrix
#'
#' @param x editmatrix object to be parsed
#' @param ... further arguments passed to or from other methods.
as.expression.editmatrix <- function(x, ...){
  return(
     tryCatch(parse(text=as.character(x)), 
         error=function(e){
             stop(paste("Not all edits can be parsed, parser returned", e$message,sep="\n"))
         }
     )
 )
}

#' \code{\link{str}} method for editmatrix object
#'
#'
#' @param object \code{\link{editmatrix}} object
#' @param ... methods to pass to other methods
#' @export
str.editmatrix <- function(object,...){
    vars <- paste(getVars(object),collapse=", ")
    if (nchar(vars) > 20 ) vars <-  paste(strtrim(vars,16),"...") 
    cat(paste("editmatrix with", nrow(object), "edits containing variables",vars,"\n"))
}
