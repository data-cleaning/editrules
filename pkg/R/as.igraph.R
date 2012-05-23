# test for presence of igraph package.
checkigraph <- function(){
    noigraph <- paste(
        "The igraph0 package is required for this function.", 
        "If you have access to an internet connection it can be installed",
        "with install.packages('igraph0')",sep="\n")
    require(igraph0) || stop(noigraph)
}



#' Convert a collection of edits to an igraph object
#' 
#' graph.editmatrix derives adjacency matrix of \code{E} and converts it to an undirected \pkg{igraph} object.
#'
#' @param weighted Should the number of variables connecting two edits be counted as weight? 
#'      Passed as \code{weighted} argument to \code{igraph::graph.adjacency}
#'
#' @rdname adjacency
#' @export
as.igraph <- function(E, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E),weighted=TRUE,...){
    stopifnot(all(vars %in% getVars(E)))
    UseMethod('as.igraph')
}

#' @rdname adjacency
#' @method as.igraph editmatrix
#' @export
as.igraph.editmatrix <- function(E, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E),weighted=TRUE, ...){
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

#' @rdname adjacency
#' @method as.igraph editarray
#' @export
as.igraph.editarray <- function(E, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E),weighted=TRUE, ...){
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


#' @rdname adjacency
#' @method as.igraph editset
#' @export
as.igraph.editset <- function(E, nodetype=c("all", "rules","vars"), rules=editnames(E), vars=getVars(E),weighted=TRUE, ...){
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

