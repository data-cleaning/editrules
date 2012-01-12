#' Replace a variable by a value in a set of edits.
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param var \code{character} with name(s) of variable(s) to substitute
#' @param value vector with value(s) of variable(s)
#' @param ... arguments to be passed to or from other methods
#' @return \code{E}, with variables replaced by values
#' @export
substValue <- function(E, var, value, ...){ 
    UseMethod("substValue")
}


#' Reduce an editmatrix by substituting a variable
#'
#' Given a set of linear restrictions \eqn{E: {\bf Ax}\odot {\bf b}} with \eqn{\odot\in\{<,\leq,==\}},
#' and matrix \eqn{{\bf A}} with columns \eqn{{\bf a}_1,{\bf a}_2,\ldots,{\bf a}_n}.
#' Substituting variable \eqn{x_j} with a value \eqn{\tilde{\bf x}_j} means setting \eqn{{\bf a}_j=0}
#' and \eqn{{\bf b}={\bf a}_j\tilde{x}_j}.
#'
#' Note that the resulting \code{\link{editmatrix}} may be inconsistent because of inconsistencies in
#' \eqn{\tilde{\bf x}}.
#'
#' @method substValue editmatrix
#' @param reduce \code{logical} should variable columns be removed from editmatrix?
#'
#' @rdname substValue 
#' @export
substValue.editmatrix <- function(E, var, value, reduce=FALSE, ...){
    v <- match(var, getVars(E), nomatch=0)
    if (any(v==0)){
        warning("Parameter var (", var[v==0], ") is not a variable of editmatrix E")
    }
    v <- v[v != 0]
    ib <- ncol(E)
    E[,ib] <- E[ ,ib] - E[ ,v]%*%value
    
    if (reduce)
        E <- E[,-v, drop=FALSE]
    else 
        E[,v] <- 0
        
    E[!isObviouslyRedundant.editmatrix(E),]    
}



#' Substitute a value in an editarray
#'
#' Only rows with \code{<var>:<value>==TRUE} are kept. In the kept rows, categories not equal to <value> are set to \code{FALSE}
#' Multiple replacements is not yet implemented. 
#'
#' @method substValue editarray
#'
#'
#' @rdname substValue
#' @export
substValue.editarray <- function(E, var, value, reduce=FALSE, ...){

    ind <- getInd(E)
    sep=getSep(E)
    A <- getArr(E)
    value <- as.character(value)
    for ( i in 1:length(var) ){
        vr <- var[i]
        vl <- value[i]
        J <- ind[[vr]]
        ii <- J[vl]
        if ( is.null(ii) || is.na(ii) ) 
            stop(paste("Variable ", vr,"not present in editarray or cannot take value",vl))

        I <- A[,ii]
        if ( reduce ){
            A <- A[ ,-setdiff(J,ii) ,drop=FALSE]
            ind <- indFromArray(A, sep)
        } else {
            A[,J] <- TRUE
        }
    }
    neweditarray(
        E = A[I,,drop=FALSE], 
        ind = ind, 
        sep = sep, 
        levels = colnames(A) 
    )
}

#' Compute index from array part of editarray
#' 
#' @param A boolean array
#' @param sep separator
#' @keywords internal
#'
indFromArray <- function(A,sep){
    if (ncol(A) == 0 ) return(list())
    cn <- colnames(A)
    l <- strsplit(cn,sep)
    V <- sapply(l,`[`,1)
#    C <- sapply(l,`[`,-1)
    C <- sapply(l,function(g) ifelse(length(g[-1])==1,g[-1],""))
    vars <- unique(V)
    ind <- lapply(vars, function(v) which(v==V))
    names(ind) <- vars
    ind <- lapply(ind, function(k){ names(k) <- C[k]; k})
    ind
}






