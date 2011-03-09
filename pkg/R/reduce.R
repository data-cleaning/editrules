#' Reduce an editmatrix by removing empty rows and columns
#'
#' @nord
#' @param E editmatrix object
reduce  <- function(E){
  stopifnot(is.editmatrix(E))
  B <- E != 0
  vars <- (colSums(B) != 0)
  edits <- (rowSums(B) != 0)
  E[edits, vars]
}