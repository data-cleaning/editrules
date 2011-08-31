
#' Check for duplicate edit rules
#'
#' @method duplicated editarray
#' @param x a \code{\link{editarray}}
#' @param ... other parameters to be passed to or from other methods.
#' @export
#' @keywords internal
duplicated.editarray <- function(x, ...) duplicated(getArr(x))



#' Check for duplicate edit rules
#'
#' @param x an \code{\link{editmatrix}}
#' @param ... options to be passed to other methods
#' @return logical vector
#' @S3method duplicated editmatrix
#' @export
#' @keywords internal
duplicated.editmatrix <- function(x,...){
    duplicated.matrix(getAb(x)) & duplicated.default(getOps(x))
}