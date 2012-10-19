#' Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for 
#' constructing a mip/lp problem.
#' @param E normalized \code{editmatrix}
#' @param xlim \code{matrix} with columns lower and upper boundary, and rows are variables (in same order as E)
#' @param prefix \code{character} used for naming dummy variables in matrix.
#' @keywords internal
softEdits <- function(E, prefix="adapt."){   # TODO change prefix into .delta.
  
  if (!nrow(E)){
    return(E)
  }
  
  n <- nrow(E)
  vars <- getVars(E)
  ops <- getOps(E)
  eq <- ops == "=="
  
  adapt <- paste(prefix,rownames(E), sep="")
  delta.plus <- paste("delta.plus.",rownames(E),sep="")
  delta.min <- paste("delta.min.",rownames(E),sep="")
  
  b <- getb(E)
  isna <- is.na(b)
    
  Ab <- cbind( getA(E)
             , diag(1, n)  # adapt
             , diag(-1, n)  # delta+
             , diag(ifelse(eq, 1, 0), n)   # delta-
             , b
             )
  # filter out the NA
  Ab <- Ab[!isna,,drop=FALSE]
  ops <- ops[!isna]
  
  colnames(Ab) <- c(getVars(E), adapt, delta.plus, delta.min, "CONSTANT")
  
  # this assures that adapt is set to 1 for b=NA  
  a <- ifelse(isna, 1, -getOption("editrules.maxdiff", 1e7))
  dp <- ifelse(isna, 0, 1)
  dm <- ifelse(eq, dp, 0)
  b <- ifelse(isna, 1, 0)
  
  Ab2 <- cbind( diag(0, nrow=n, ncol=length(vars))
              , diag(a, n) # adapt
              , diag(dp,n)   # delta+
              , diag(dm, n) # delta-
              , b
              )
  rownames(Ab2) <- delta.plus #should not be equal to rownames(E)
  ops2 <- ifelse(isna, "==", "<=")
    
  seE <- neweditmatrix(rbind(Ab,Ab2), ops=c(ops,ops2))
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
# xlim <- cbind(l=c(x=-1,y=-5), u=c(2,10))
# xlim
# #upperBounds(E, xlim)
# (se <- softEdits(E,xlim))
