#' Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for 
#' constructing a mip/lp problem.
#' @param E normalized \code{editmatrix}
#' @param xlim \code{matrix} with columns lower and upper boundary, and rows are variables (in same order as E)
#' @param prefix \code{character} used for naming dummy variables in matrix.
#' @keywords internal
softEdits <- function(E, xlim, prefix=".se."){
  if (!nrow(E)){
    return(E)
  }
  eq <- getOps(E) == "=="
  
  seNms <- dummies <- paste(prefix, rownames(E), sep="")
  if (any(eq)){ # replace each equality constraint by 2 inequalities
    Ab <- getAb(E[eq,])
    Ab <- rbind(Ab, -Ab)
    ops <- rep("<=", nrow(Ab))
    E <- c(E[!eq,], neweditmatrix(Ab, ops))
    seNms <- c(dummies[!eq], rep(dummies[eq], 2))
  }
  
  # E now contains only inequalities (i.e. "<" and "<=")
  ub <- getUpperBounds(E,xlim)
  A <- getA(E)
  
  seA <- sapply(dummies, function(d) { ifelse(d == seNms, ub[,3], 0)})
  seA <- cbind(A,seA)
  colnames(seA) <- c(colnames(A), dummies)
  
  binvars <- sapply(colnames(seA), `%in%`, dummies)
  seE <- as.editmatrix(seA, ub[,2], getOps(E), binvars=binvars)
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
# 
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
