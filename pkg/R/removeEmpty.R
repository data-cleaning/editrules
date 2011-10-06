
#' Reduce an (edit)matrix by removing empty rows and columns
#'
#' @keywords internal
#' @param E an object that is coerceable to a \code{matrix}
removeEmpty  <- function(E){
  if (!is.editmatrix(E)) stop("Argument is not an editmatrix")
  m <- as.matrix(E)
  B <- m != 0
  v <- 1:(ncol(m)-1)
  vars <- which(colSums(B[,v,drop=FALSE]) != 0)
  edits <- (rowSums(B) != 0)
  E[edits,c(vars,ncol(m)) , drop=FALSE]
}





