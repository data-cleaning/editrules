#' Expand an edit expression
#'
#' Often many numeric variables have the same constraints. \code{expandEdits} is
#' a utility function to define edits for multiple variables. See the examples for the syntax.
#' @param s edit expression
#' @param prefix prefix for variables to be expanded
#' @param useSum if \code{TRUE} sum expression will be expanded
#' @param asExpression if \code{TRUE} an \code{\link{expression}} will be returned in stead of a \code{character}
#' @param ... variables used in the expansion
#' @return \code{character} or \code{expression} vector with expanded expressions
#' @export
#' @example ../examples/expandEdits.R
expandEdits <- function(s, prefix="_", useSum=TRUE, asExpression=FALSE, ...){  
  #TODO replace special regex character in prefix with escaped character.
  
  if (length(s) > 1){
    return(lapply(s, expand, prefix=prefix, useSum=useSum, ...))
  }
    
  l <- list(...)

  if (useSum) {
    sumnms <- paste("sum", names(l), sep=prefix)
    sumregex1 <- paste(sumnms, "\\((.+?)\\)", sep="")
    sumregex2 <- paste(sumnms, "\\((.+?)\\).+", sep="")
    vars <- names(l)
    for (i in seq_along(vars)){      
      if (length(grep(sumregex2[i], s))){
        sumvars <- sub(sumregex2[i], "\\1", s)
        sumvars <- do.call(expandEdits, append(list(s=sumvars), l[vars[i]]))
        sumvars <- paste(sumvars, collapse=" + ")
        s <- sub(sumregex1[i], sumvars, s)
        l[[vars[i]]] <- NULL
      }
    }
  }
  
  varnms <- paste(prefix,names(l), sep="")
  for (i in seq_along(l)){
    s <- sapply(l[[i]], function(j) gsub(varnms[i],j,s))    
  }
  
  if (is.array(s)) {
    dimnames(s) <- l
  } else if (is.vector(s) && length(l)){
    names(s) <- l[[1]]
  }
  if (asExpression){
    parse(text=s)
  } else {
    s
  }
}

## quick test
# expand("x_i < y_j", i=1:3,j=2:3)
# expand("sum_i(x_i) == y", i=1:3)
# expand("sum_i(x_i) + y_j==z_j", i=1:2, j=3:4)
# 
# expand(c("a_i","b_j"), i=2:3, j=5:6)