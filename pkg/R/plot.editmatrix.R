#' Graphical representation of editmatrix
#' 
#' Depending on the chosen \code{nodetype}, this function can plot
#' three types of graphs based on an edit set.
#'
#'\itemize{
#'\item{If \code{nodetype="all"} (default), the full bipartite graph is plotted. Each
#' variable is represented by a square node while each edit is represented by a circular
#' node. An edge is drawn when a variable occurs in an edit.}
#'
#' \item{If \code{nodetype="vars"} the variable graph is drawn. Each node represents a
#' variable, and an edge is drawn between two nodes if the variables occur together in at
#' least one edit. The edge width relates to the number of edits connecting two variables.}
#'
#' \item{If \code{nodetype="rules"} the rule graph is drawn. Each node represents an edit
#' rule and an edge is drawn between two nodes if they share at least one variable. The 
#' edge width relates to the number of edits connecting the two edit rules.}
#'}
#'
#' The boolean vectors \code{violated} and \code{adapt} can be used to color violated 
#' edits or variables which have to be adapted.
#' 
#' The function works by coercing an editmatrix to an \code{igraph} object, and therefore
#' relies on the plotting capabilities of the igraph package. For more finetuning,
#' use \code{\link{as.igraph}} and see \code{?igraph.plotting}.
#'
#'
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
#' @param ... further  arguments to be passed to plot. 
#' 
#' @example ../examples/graph.R
#'
#' @seealso \code{\link{as.igraph}}, \code{\link{adjacency}}, \code{igraph.plotting}
#'
#' @references
#'  Csardi G, Nepusz T: The igraph software package for complex network
#'  research, InterJournal, Complex Systems 1695. 2006. http://igraph.sf.net
#'
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
plotEditGraph <- function(
    x,
    nodetype, 
    rules, 
    vars, 
    violated, 
    adapt, 
    rulecolor, 
    varcolor,
    edgecolor, 
    violatedcolor, 
    adaptcolor,...){

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



