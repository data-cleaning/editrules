#' concatenate editmatrix
#' @method c editmatrix
c.editmatrix <- function(...){
  ems <- list(...)
  ems <- lapply(ems, as.editmatrix)
  vars <- unique(unlist(lapply(ems, getVars)))
  
  A <- lapply(ems, function(E){
    a <- matrix(0, nrow=nrow(E), ncol=length(vars), dimnames=list(NULL, vars))
    a[, getVars(E)] <- getA(E)
    a
  })
  A <- do.call(rbind, A)
  
  ops <- unlist(lapply(ems, getOps))
  b <- unlist(lapply(ems, getb))
  
  as.editmatrix(A=A, ops=ops, b=b)
}

# # quick test
# E1 <- editmatrix(expression(x>1))
# E2 <- editmatrix(expression(x <2, y > 2))
# c(E1,E2)
