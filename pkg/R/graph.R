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
#' @param vars variables connecting edits
#' @param ... further  arguments to be passed to plot. 
#' 
#' @example ../examples/graph.R
#' @export
plot.editmatrix <- function(x,vars=getVars(x),...){
    checkigraph()
    plotEditGraph(x,vars=vars,...)
}

#' plot method for editarray
#'
#' @rdname plot.editmatrix
#' @method plot editarray
#' @export
plot.editarray <- function(x,vars=getVars(x),...){
    checkigraph()
    plotEditGraph(x,vars=vars,...)

}

# internal edit set plotter
plotEditGraph <- function(x,vars,...){
    g <- as.igraph(x,vars=vars)
    w <- E(g)$weight
    if (sum(w) > 0 ){
        weight <- 1 + w + (max(w) - min(w))/max(w)
    } else {
        weight <- 2
    }
    plot(g,
        layout=layout.fruchterman.reingold,
        vertex.label=V(g)$name,
        vertex.label.font=2,
        vertex.color="#FFFD92",
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



