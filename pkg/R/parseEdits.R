#' Parse a character vector of edits
#'
#' This function wraps the native \code{\link{parse}} function in a \code{\link{tryCatch}}.
#' The function is \code{editrules} internal. It tries to give a meaningfull error message when
#' parsing fails for some reason.
#'
#' @param E \code{character}
#' @param type optional filter for type of edit, may be \code{"all"}, \code{"num"}
#' \code{"cat"} or \code{"mix"}
#' @return The edits in \code{E} parsed to R expressions.
#'
#' @export
parseEdits <- function(E, type=c("all", "num", "cat", "mix")){
     edits <- 
        tryCatch(parse(text=E), 
            error=function(e){
                stop(paste("Not all edits can be parsed, parser returned", e$message,sep="\n"))
            })
     type <- match.arg(type)
     if (type=="all"){
       return(edits)
     } 
     return(edits[editTypes(edits) == type])
}

NUMCMP <- c("==","<","<=",">",">=")
NUMOPS <- c("+","-","*")

#' Parse a numerical edit expression 
#'
#' Parse a numerical edit expression into a named \code{numeric}.
#' The \code{names} are the names of the variables
#' @param e a valid R expression
#' @keywords internal
parseNum <- function(e){
  if (!isNum(e)){
     stop(paste("Invalid edit rule:", e))
  }
  wgt <- retrieveCoef(e)
  # simplify the coefficients by summing them
  tapply(wgt, names(wgt), sum)
}

CATCMP <- c("==", "!=", "%in%")

#' Parse a categorical edit expression 
#'
#' @param x a valid R expression
#' @param val logical (scalar)
#' @param edit logical (vector)
#' @param sep edit separator
#' @param useLogical (logical), should logicals be treated as a factor or as a logical?
#' @keywords internal
parseCat <- function(x, val=NA, edit=logical(0), sep=":", useLogical=FALSE){
    if ( length(x) == 1 ) {
       # corner case: the always FALSE edit (array must be TRUE at every category)
       if ( is.na(val) && !x[[1]] ) return(NULL)
       var <- if (useLogical) as.character(x)
              else paste(x,"TRUE",sep=sep)
       edit[var] <- val
       return(edit)
    }
    op <- as.character(x[[1]])
    if ( op == "if" ){
        edit <- parseCat(x[[2]],TRUE,  edit, sep, useLogical)
        edit <- parseCat(x[[3]],FALSE, edit, sep, useLogical)
    } else if ( op %in% c("(","{") ){
        edit <- parseCat(x[[2]], val,  edit, sep, useLogical)
    } else if ( op %in% c("%in%","==") ){
        cat <- eval(x[[3]])
        if ( is.na(val) && op == "==" ) val <- TRUE
        if (is.logical(cat) && useLogical){
            var <- as.character(x[[2]])
            if (!cat) val <- !val
        } else {
            var <- paste(x[[2]],cat,sep=sep)
        }
        edit[var] <- val
    } else if (op == "!=") {
        cat <- eval(x[[3]])
        if (is.logical(cat) && useLogical){
          var <- as.character(x[[2]])
          if (!cat) val <- !val
        } else{
          var <- paste(x[[2]],cat,sep=sep)
        }
        edit[var] <- !val
    } else if (op == "!") {
        edit <- parseCat(x[[2]],!val,  edit, sep, useLogical)
    } else if (op == "&&"){
        if (is.na(val))
           val <- TRUE
        if (val == FALSE){
            stop("Operator '&&' not allowed in 'then' clause")
        }
        edit <- parseCat(x[[2]],val, edit, sep, useLogical)
        edit <- parseCat(x[[3]],val, edit, sep, useLogical)
    } else if (op %in% c("||","|")){
        if (is.na(val))
           val <- FALSE
        if (val == TRUE){
             stop("Operator '||' not allowed in 'if' clause")
        }
        edit <- parseCat(x[[2]],val, edit, sep, useLogical)
        edit <- parseCat(x[[3]],val, edit, sep, useLogical)
    } else {
        stop("Operator '",op,"' not implemented")
    }
    edit
}

parseTree <- function(expr,prefix=NULL){
   if (length(expr) == 1){
      indent <- paste("[", prefix,"]", sep="", collapse="")
      cat(indent, expr,"\n")
   }
   else {
       for (i in 1:length(expr)){
          parseTree(expr[[i]], c(prefix,i)) 
       }
   }
}

hasNum <- function(e){
  if (length(e) == 1){
    return(is.numeric(e))
  }
  op <- deparse(e[[1]])
  if (length(e) == 2){
    return (op %in% NUMOPS || hasNum(e[[2]]))
  }
  if (length(e) == 3){
    return(op %in% NUMOPS || hasNum(e[[2]]) || hasNum(e[[3]]))
  }
}

# very basic test for numerical edit
isNum <- function(e){
  if (length(e) != 3) 
    return(FALSE)  
  cmp <- deparse(e[[1]])
  return(cmp %in% NUMCMP)
}

#' basic test for type of edit
#' @param edts \code{expression}
#  @keywords internal
editTypes <- function(edts){
  ops <- sapply(edts, function(e){deparse(e[[1]])})
  
  type  <- ifelse(ops %in% NUMCMP, "num", "cat")
  # todo add check for "=="

  iff <- ops == "if"
  mix <- sapply(edts[iff], hasNum)
  type[iff] <- ifelse(mix, "mix", "cat")
  as.factor(type)
}
  
retrieveCoef <- function(e, co=1){
   #stopifnot(is.language(e))
   if (length(e) == 1){
     if (is.numeric(e)){
        l <- co*e   #the resulting matrix is augmented so b has a -
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
	  } else if (op == "-"){
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
      if ( op %in% c(NUMCMP, "-")){
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
