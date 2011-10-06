# test for presence of igraph package.
checkigraph <- function(){
    noigraph <- paste(
        "The igraph package is required for this function.", 
        "If you have access to an internet connection it can be installed",
        "with install.packages('igraph')",sep="\n")
    require(igraph) || stop(noigraph)
}



#' Convert an editmatrix to an igraph object
#' 
#' graph.editmatrix derives adjacency matrix of \code{E} and converts it to an undirected igraph object.
#'
#' @param weighted Should the number of variables connecting two edits be counted as weight? 
#'      Passed as \code{weighted} argument to \code{igraph::graph.adjacency}
#'
#' @rdname adjacency
#' @export
as.igraph <- function(E, nodetype=c("all", "rules","vars"), rules=rownames(E), vars=getVars(E),weighted=TRUE,...){
    stopifnot(all(vars %in% getVars(E)))
    UseMethod('as.igraph')
}

#' method for converting editmatrix to igraph object
#'
#' @rdname adjacency
#' @method as.igraph editmatrix
#' @export
as.igraph.editmatrix <- function(E, nodetype=c("all", "rules","vars"), rules=rownames(E), vars=getVars(E),weighted=TRUE, ...){
    checkigraph()
    nodetype <- match.arg(nodetype)
    a <- adjacency(E=E, nodetype=nodetype, rules=rules, vars=vars, ...)
    g <- graph.adjacency(
          a, 
          weighted=weighted,
          mode = 'undirected'
        )
    # $type is handy for bipartite graph function in igraph...
    V(g)$type <- V(g)$vars <- attr(a, "vars")
    g
}

#' method for converting editmatrix to igraph object
#'
#' @rdname adjacency
#' @method as.igraph editarray
#' @export
as.igraph.editarray <- function(E, nodetype=c("all", "rules","vars"), rules=rownames(E), vars=getVars(E),weighted=TRUE, ...){
    checkigraph()
    nodetype <- match.arg(nodetype)
    a <- adjacency(E=E, nodetype=nodetype, rules=rules, vars=vars, ...)
    g <- graph.adjacency(
          a, 
          weighted=weighted,
          mode = 'undirected'
        )
    V(g)$type <- V(g)$vars <- attr(a, "vars")
    g
}
