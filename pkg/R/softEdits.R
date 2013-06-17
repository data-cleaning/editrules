#' Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for 
#' constructing a mip/lp problem.
#' @param E normalized \code{editmatrix}
#' @param prefix \code{character} used for naming dummy variables in matrix.
#' @keywords internal
softEdits <- function(E, prefix="delta."){
  
  if (!nrow(E)){
    return(E)
  }
  
  M <- 1e7
  
  n <- nrow(E)
  vars <- getVars(E)
  ops <- getOps(E)
  
  adapt <- paste(prefix,rownames(E), sep="")
  
  A <- getA(E)
  b <- getb(E)
  isna <- is.na(b)
  eq <- (ops == "==") & !isna
  
  Ab <- cbind( A
             , diag(-M, n)
             , b
             )[!isna,,drop=FALSE]
  
  # copy the equality constraints
  Ab_eq <- if(any(eq)){
           cbind( -A
                , diag(-M,n)
                , -b
                )[eq,,drop=FALSE]
           }
  
  # NA's must be changed.
  Ab_na <- if(any(isna)){
              cbind( diag(0, n)
                , diag(1, n)
                , 1
                )[isna,,drop=FALSE]
           }
  # TODO cleanup this code
  #print(list(Ab=Ab, Ab_eq=Ab_eq, Ab_na=Ab_na))
  Ab <- rbind(Ab, Ab_eq, Ab_na)
  rownames(Ab) <- make.unique(rownames(Ab))
  ops <- c(ops[!isna], ops[eq], rep("==", sum(isna)))
  ops <- gsub("==", "<=", ops)
  
  #print(list(Ab=Ab, ops=ops))
  
  
  colnames(Ab) <- c(getVars(E), adapt, "CONSTANT")
  
  seE <- neweditmatrix(Ab, ops=ops)
  seE
}

#' Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for 
#' constructing a mip/lp problem.
#' @param E normalized \code{editmatrix}
#' @param prefix \code{character} used for naming dummy variables in matrix.
#' @keywords internal
softEdits.cateditmatrix <- function(E, prefix=".se."){
  if (!nrow(E)){
    return(E)
  }
  eq <- getOps(E) == "=="
  
  dummies <- paste(prefix, rownames(E), sep="")
  
  seA <- diag(ifelse(eq, 1, -1))
  colnames(seA) <- dummies
  seA <- cbind(getA(E), seA)
  
  binvars <- sapply(colnames(seA), is.character)
  seE <- as.editmatrix(seA, getb(E), getOps(E), binvars=binvars)
  seE
}

#quick tests

# E <- editmatrix(expression( x - y < 2
#                           , x + y < 5
#                           , x + y == 3
#                           )
#                )
# 
# (se <- softEdits(E))
