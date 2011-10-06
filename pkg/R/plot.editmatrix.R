#' Graphical representation of editmatrix
#' 
#' This function computes the edge graph of an editmatrix and plots
#' it with some sensible defaults. Each (labeled) vertex corresponds to
#' an edit, while an edge indicates that its vertices share at least one
#' variable. The linewidht of the plotted edge relates to the number of
#' variables connecting the edits. 
#' 
#' For more finetuning, see \code{\link{as.igraph}}, and \code{igraph.plotting}
#'
#' @method plot editmatrix 
#'
#' @param x object of class \code{\link{editmatrix}}
#' @param nodetype plot edits, variables or both?
#' @param rules selection of edits
#' @param vars selection of variables
#' @param ... further  arguments to be passed to plot. 
#' 
#' @example ../examples/graph.R
#' @export
plot.editmatrix <- function(x,nodetype="all", rules=rownames(x), vars=getVars(x),...){
    checkigraph()
    plotEditGraph(x, nodetype=nodetype, rules=rules, vars=vars,...)
}

#' plot method for editarray
#'
#' @rdname plot.editmatrix
#' @method plot editarray
#' @export
plot.editarray <- function(x,nodetype="all", rules=rownames(x), vars=getVars(x),...){
    checkigraph()
    plotEditGraph(x, nodetype=nodetype, rules=rules, vars=vars,...)
}

# internal edit set plotter
plotEditGraph <- function(x,nodetype, rules, vars,...){
    g <- as.igraph(x,nodetype, rules, vars=vars)
    w <- E(g)$weight
    if (sum(w) > 0 ){
        weight <- 1 + w + (max(w) - min(w))/max(w)
    } else {
        weight <- 2
    }
    vars <- V(g)$vars
    if (is.null(vars)){
      vars <- rep(FALSE, length(V(g)))
    }
    shape <- ifelse(vars, "rectangle", "circle")
    color <- ifelse(vars, "#92FDFF", "#FFFD92")
    plot(g,
        layout=layout.fruchterman.reingold,
        vertex.label=V(g)$name,
        vertex.label.font=2,
        vertex.color=color,
        vertex.shape=shape,
        vertex.frame.color="#7180D6",
        vertex.label.family="",
        edge.width= weight,
        edge.color="#7180D6",
        ...)
}


#library(editrules)
#set.seed(1)
#data(edits)
#plot(editmatrix(edits))



