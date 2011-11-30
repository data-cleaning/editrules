#' concatenate editmatrix
#' @method c editmatrix
#' @export
#' @param ... \code{editmatrix} objects to be concatenated.
#' @return \code{editmatrix}
c.editmatrix <- function(...){
  ems <- list(...)
  ems <- lapply(ems, as.editmatrix)
  vars <- sort(unique(unlist(lapply(ems, getVars))))
  
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

#' concatenate editarray
#' @method c editarray
#' @export
#' @param ... \code{editarray} objects to be concatenated.
#' @return \code{editarray}
c.editarray <- function(...){
  ems <- list(...)
  
  lvls <- sort(unique(unlist(lapply(ems, getlevels))))
  seps <- unlist(sapply(ems, getSep))
  #TODO handle seperators that are not equal to ":"
  stopifnot(all(seps==":"))
  
  B <- lapply(ems, function(E){
    a <- matrix(TRUE, nrow=nrow(E), ncol=length(lvls), dimnames=list(NULL, lvls))
    a[, getlevels(E)] <- getArr(E)
    a
  })
  
  B <- do.call(rbind, B)
  cats <- sub("^.+:", "", lvls)
  vars <- sub(":.+$", "", lvls)
  ind <- seq_along(lvls)
  names(ind) <- cats
  ind <- split(ind, vars)
  neweditarray(B, ind, sep=":")
}
# # quick test
# E1 <- editmatrix(expression(x>1))
# E2 <- editmatrix(expression(x <2, y > 2))
# c(E1,E2)

#E1 <- editarray(expression(A %in% c("a1", "a2"), B %in% c("b1", "b2"), if (A=='a1') B == 'b1'))
#E2 <- editarray(expression(B %in% c("b1", "b3"), C %in% c("c1", "c2"), if (B=='b1') C == 'c1'))
#c(E1,E2)