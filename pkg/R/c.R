
#' @method c editmatrix
#' @rdname editmatrix
#' @export
#' 
c.editmatrix <- function(...){
  ems <- list(...)
  ems <- ems[!sapply(ems,is.null)]

  ems <- lapply(ems, as.editmatrix)
  vars <- sort(unique(unlist(lapply(ems, getVars.editmatrix))))
  
  A <- lapply(ems, function(E){
    a <- matrix(0, nrow=nrow(E), ncol=length(vars), dimnames=list(NULL, vars))
    a[, getVars(E, type="colnames")] <- getA(E)
    a
  })
  A <- do.call(rbind, A)
  
  ops <- unlist(lapply(ems, getOps))
  b <- unlist(lapply(ems, getb))
  
  as.editmatrix(A=A, ops=ops, b=b)
}

#' 
#' @rdname editarray
#' @method c editarray
#' @export
#' 
c.editarray <- function(...){
  ems <- list(...)
  ems <- ems[!sapply(ems,is.null)]
  
  lvls <- sort(unique(unlist(lapply(ems, getlevels))))
  seps <- unlist(sapply(ems, getSep))
  #TODO handle seperators that are not equal to ":"
  stopifnot(all(seps==":"))
  
  B <- lapply(ems, function(E){
    a <- matrix(TRUE, nrow=nrow(E), ncol=length(lvls), dimnames=list(rownames(E), lvls))
    a[, getlevels(E)] <- getArr(E)
    a
  })
  
  B <- do.call(rbind, B)
  cats <- sub("^.+:", "", lvls)
  vars <- sub(":.+$", "", lvls)
  ind <- seq_along(lvls)
  names(ind) <- cats
  ind <- split(ind, vars)
  neweditarray(B, ind, names=rownames(B), sep=":")
}


#' @method c editset
#' @rdname editset
#' @export
#' 
c.editset <- function(...){
    editset( unlist(lapply(list(...), as.character)) )
}

