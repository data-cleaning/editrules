
#' Derive adjecency matrix from set of edits
#'
#' A set of edits can be represented in a graph where every vertex is
#' an edit. Two vertices are connected if they have at least one variable
#' in \code{vars} in common.
#' 
#' \code{adjacency} returns the adjacency matrix. The elements of the matrix
#' count the number of variables shared by the edits indicated in the row- and 
#' column names. The adjacency matrix can be converted to an igraph object with 
#' \code{graph.adjacency}from the \code{igraph} package.
#'
#' \code{as.igraph} converts a set of edits to an \code{igraph} object directly.
#'
#'
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param vars variables linking the edits
#' @param ... arguments to be passed to or from other methods
#'
#' @return the adjacency matrix of edits in \code{E} with resect to 
#'  the variables in \code{vars}
#'
#' @example ../examples/graph.R
#'
#' @seealso \code{\link{plot.editmatrix}}, \code{\link{plot.editarray}}
#' @export  
adjacency <- function(E, vars=getVars(E),...){
    stopifnot( all(vars %in% getVars(E)) )
    UseMethod('adjacency')
}

#' editmatrix method for calculating adjacency matrix
#'
#'
#' @rdname adjacency
#' @method adjacency editmatrix
#' @export
adjacency.editmatrix <- function(E,vars=getVars(E),...){
    A <- abs(sign(getA(E)))
    adjec(A,vars=vars)
}


#' editmatrix method for calculating adjacency matrix
#'
#'
#' @rdname adjacency
#' @method adjacency editarray
#' @export
adjacency.editarray <- function(E,vars=getVars(E),...){
    A <- contains(E)
    adjec(A,vars=vars)
}

# derive adjacency from 1/0 or boolean matrix.
# Internal loops only, but nrow(A)^2 memory complexity. 
# future optimization options: sparse matrices, lower/upper triangle only.
adjec <- function(A,vars){
    I <- rep(1:nrow(A), times=nrow(A))
    J <- rep(1:nrow(A), each=nrow(A)) 
    V <- matrix(
            rowSums(A[I,vars,drop=FALSE] & A[J,vars,drop=FALSE]),
            nrow=nrow(A),
            dimnames=list(rownames(A), rownames=rownames(A))
    )
    diag(V) <- 0
    V
}



