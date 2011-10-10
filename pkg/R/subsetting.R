
#' Row index operator for \code{editmatrix}
#'
#' Use this operator to select edits from an editmatrix or editarray object.
#'
#' @method [ editmatrix 
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

#' Row index operator for \code{editarray}
#'
#' @method [ editarray
#' @rdname subsetting
#' @export
#'
`[.editarray` <- function(x, i, j, ...){
    A <- getArr(x)[i,j,drop=FALSE]
    sep <- getSep(x)
    ind <- indFromArray(A,sep)
    neweditarray(E=A, ind=ind, sep=sep, names=getnames(x)[i],levels=getlevels(x)[j])
}


