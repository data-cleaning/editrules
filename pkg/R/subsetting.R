
#' Row index operator for \code{editmatrix} or \code{editarray}
#'
#' Use this operator to select edits from an editmatrix or editarray object.
#'
#' @usage `[.editmatrix`(x,i,j,...)
#' @param x an object of class \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param i the row index in the edit matrix
#' @param j the column index in the edit matrix
#' @param ... arguments to be passed to other methods. Currently ignored.
#' @rdname subsetting
#' @export
`[.editmatrix` <- function(x, i, j, ...){
    E <- neweditmatrix(
        A = as.matrix(x)[i, j, drop=FALSE],
        ops = getOps(x)[i]
    )
    attr(E,"H") <- attr(x,"H")[i, , drop=FALSE]
    attr(E,"h") <- attr(x,"h")
    E
}

# Row index operator for \code{editarray}
# 
# @rdname subsetting
# @export
#
#`[.editarray` <- function(x, i, j, ...){
#    neweditarray(getArr(x)[i, j, drop=FALSE], getInd(x), sep=getSep(x), names=getnames(x),levels=getlevels(x))
#}


