# test for presence of igraph package.
checkigraph <- function(){
    noigraph <- paste(
        "The igraph package is required for this function.", 
        "If you have access to an internet connection it can be installed",
        "with install.packages('igraph')",sep="\n")
    require(igraph) || stop(noigraph)
}


#' @method as.igraph editmatrix
#' @param x An object of class \code{\link{editmatrix}}, \code{\link{editarray}} or \code{\link{editset}}
#' @param weighted see \code{\link[igraph]{graph.adjacency}}
#' @export
#' @rdname adjacency
as.igraph.editmatrix <- function(x, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E), weighted=TRUE, ...){
    checkigraph()
    nodetype <- match.arg(nodetype)
    a <- adjacency(E=x, nodetype=nodetype, rules=rules, vars=vars, ...)
    g <- graph.adjacency(
          a, 
          weighted=weighted,
          mode = 'undirected'
        )
    # $type is handy for bipartite graph function in igraph...
    V(g)$type <- V(g)$vars <- attr(a, "vars")
    g
}

#' @method as.igraph editarray
#' @export
#' @rdname adjacency
as.igraph.editarray <- function(x, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E),weighted=TRUE, ...){
    checkigraph()
    nodetype <- match.arg(nodetype)
    a <- adjacency(E=x, nodetype=nodetype, rules=rules, vars=vars, ...)
    g <- graph.adjacency(
          a, 
          weighted=weighted,
          mode = 'undirected'
        )
    V(g)$type <- V(g)$vars <- attr(a, "vars")
    g
}


#' @method as.igraph editset
#' @export
#' @rdname adjacency
as.igraph.editset <- function(x, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E),weighted=TRUE, ...){
    checkigraph()
    nodetype <- match.arg(nodetype)
    a <- adjacency(E=x, nodetype=nodetype, rules=rules, vars=vars, ...)
    g <- graph.adjacency(
          a, 
          weighted=weighted,
          mode = 'undirected'
        )
    V(g)$type <- V(g)$vars <- attr(a, "vars")
    g
}

