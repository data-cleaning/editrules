#' Graphical representation of editmatrix
#' 
#' This function computes the edge graph of an editmatrix and plots
#' it with some sensible defaults. Each (labeled) vertex corresponds to
#' an edit, while an edge indicates that its vertices share at least one
#' variable. The linewidht of the plotted edge relates to the number of
#' variables connecting the edits. 
#' 
#' The boolean vector variables \code{adapt} and \code{violated} can be used to color
#' variable or edit nodes.
#'
#' For more finetuning, see \code{\link{as.igraph}}, and \code{igraph.plotting}
#'
#' @method plot editmatrix 
#'
#' @param x object of class \code{\link{editmatrix}}
#' @param nodetype \code{'rules'}, \code{'vars'} or \code{'all'}.
#' @param rules selection of edits
#' @param vars selection of variables
#' @param violated Boolean vector of \code{nrow(E)}. Ingnored when \code{nodetype='vars'}
#' @param adapt Boolean vector of \code{length(getVars(E))}. Ignored when \code{nodetype='rules'}
#' @param edgecolor Color of edges and node frames 
#' @param rulecolor Color of rule nodes (ignored when \code{nodetype='vars'})
#' @param varcolor Color of variable nodes (ignored when \code{nodetype='rules'})
#' @param violatedcolor Color of nodes corresponding to violated edits (ignored when \code{nodetype='vars'})
#' @param adaptcolor Color of nodes corresponding to variables to adapt (ignored when \code{nodetype='rules'})
#'
#'
#'
#' @param ... further  arguments to be passed to plot. 
#' 
#' @example ../examples/graph.R
#' @export
plot.editmatrix <- function(x,
    nodetype="all", 
    rules=rownames(x), 
    vars=getVars(x), 
    violated=logical(nrow(x)), 
    adapt=logical(length(getVars(x))),
    edgecolor='steelblue',
    rulecolor='khaki1',
    varcolor='lightblue1',
    violatedcolor='sienna1',
    adaptcolor='sienna1',
    ...){
    checkigraph()
    plotEditGraph(
        x, 
        nodetype=nodetype, 
        rules=rules, 
        vars=vars,
        violated=violated,
        adapt=adapt,
        edgecolor=edgecolor,
        rulecolor=rulecolor,
        varcolor=varcolor,
        violatedcolor=violatedcolor,
        adaptcolor=adaptcolor,
        ...)
}

#' plot method for editarray
#'
#' @rdname plot.editmatrix
#' @method plot editarray
#' @export
plot.editarray <- function(
    x,
    nodetype="all", 
    rules=rownames(x), 
    vars=getVars(x), 
    adapt=logical(length(getVars(x))),
    violated=logical(nrow(x)), 
    edgecolor='steelblue',
    rulecolor='khaki1',
    varcolor='lightblue1',
    violatedcolor='sienna1',
    adaptcolor='sienna1',
    ...){
    checkigraph()
    plotEditGraph(
        x, 
        nodetype=nodetype, 
        rules=rules, 
        vars=vars,
        violated=violated,
        adapt=adapt,
        edgecolor=edgecolor,
        rulecolor=rulecolor,
        varcolor=varcolor,
        violatedcolor=violatedcolor,
        adaptcolor=adaptcolor,
        ...)
}

# internal edit set plotter
plotEditGraph <- function(x,nodetype, rules, vars, violated, adapt, rulecolor, varcolor,edgecolor, violatedcolor, adaptcolor,...){

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
    color <- ifelse(vars, varcolor, rulecolor)

    if ( nodetype != 'rules' ) color[which(vars)[adapt]] <- adaptcolor
    if ( nodetype != 'vars' ) color[which(!vars)[violated]] <- violatedcolor
    plot(g,
        layout=layout.fruchterman.reingold,
        vertex.label=V(g)$name,
        vertex.label.font=2,
        vertex.color=color,
        vertex.shape=shape,
        vertex.frame.color=edgecolor,
        vertex.label.family="",
        edge.width= weight,
        edge.color=edgecolor,
        ...)
}


#library(editrules)
#set.seed(1)
#data(edits)
#plot(editmatrix(edits))



