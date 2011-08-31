
#' Reduce an (edit)matrix by removing empty rows and columns
#'
#' @keywords internal
#' @param E an object that is coerceable to a \code{matrix}
removeEmpty  <- function(E){
  if (!is.editmatrix(E)) stop("Argument is not an editmatrix")
  m <- as.matrix(E)
  B <- m != 0
  vars <- (colSums(B) != 0)
  edits <- (rowSums(B) != 0)
  E[edits, vars, drop=FALSE]
}





