#' Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for 
#' constructing a mip/lp problem.
#' @param E normalized \code{editmatrix}
#' @param xlim \code{matrix} with columns lower and upper boundary, and rows are variables (in same order as E)
#' @param prefix \code{character} used for naming dummy variables in matrix.
#' @keywords internal
softEdits <- function(E, xlim, prefix="adapt."){   # TODO change prefix into .delta.
  
  if (!nrow(E)){
    return(E)
  }
  
  n <- nrow(E)
  vars <- getVars(E)
  ops <- getOps(E)
  eq <- ops == "=="
  
  adapt <- paste(prefix,rownames(E), sep="")
  delta.plus <- paste(".delta.plus.",rownames(E),sep="")
  delta.min <- paste(".delta.min.",rownames(E),sep="")
  
  Ab <- cbind( getA(E)
             , diag(0, n)  # adapt
             , diag(-1, n)  # delta+
             , diag(ifelse(eq, 1, 0), n)   # delta-
             , getb(E)
             )
  
  colnames(Ab) <- c(getVars(E), adapt, delta.plus, delta.min, "CONSTANT")
  
  Ab2 <- cbind( diag(0, nrow=n, ncol=length(vars))
              , diag(-getOption("editrules.maxdiff", 1e7), n) # adapt
              , diag(1,n)   # delta+
              , diag(ifelse(eq,1,0), n) # delta-
              , 0
              )
  rownames(Ab2) <- delta.plus #should not be equal to rownames(E)
  ops2 <- rep("<=",n)
    
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
