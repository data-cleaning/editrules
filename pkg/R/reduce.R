#' Reduce an (edit)matrix by removing empty rows and columns
#'
#' @nord
#' @param E an object that is coerceable to a \code{matrix}
removeEmpty  <- function(E){
  m <- as.matrix(E)
  B <- m != 0
  vars <- (colSums(B) != 0)
  edits <- (rowSums(B) != 0)
  E[edits, vars, drop=FALSE]
}

#' Reduce an editmatrix by removing redundant rows
#'
#' matrix should be normalized, i.e. the operators should have similar sign
#' @nord
#' @param E editmatrix object
reduceMatrix <- function(E){
   m <- getMatrix(E)
   C <- getC(E)
   ops <- getOps(E)
   
   # check if E is equality/inequality matrix
   
   #normalize on sum of abs coefficients
   
   #sort the matrix
   
   # detect if coefficient row e is equal to row e+1
   E
}

#' Reduce an editmatrix by setting a variable to a value
#'
#' @nord
#' @param E \code{editmatrix} object
#' @param var \code{character} with name of variable
#' @param value \code{numeric} with value of variable
#' @return reduced edit matrix or NULL if \code{value} is invalid with editmatrix
fillVariable <- function(E, var, value){
   v <- match(var, getVars(E), nomatch=0)
   if (v==0){
      stop("Parameter var (", var, ") is not a variable of editmatrix E")
   }
   
   m <- getMatrix(E)
   varedits <- m[,v] != 0
   varedits <- rowSums(m[varedits,,drop=FALSE]) == 1
   
   if (any(varedits)){
   
      ops <- getOps(E)
      C <- getC(E)
      
      check <- sapply( which(varedits)
                     , function(i){
                         do.call(ops[i], 0, C[i])
                       }
                     )
      if (!all(check)){
         return(NULL)
      }
   }
   attr(E,"C") <- getC(E) - (m[,v] * value)   
   E[,v] <- 0
   
   removeEmpty(E)
}