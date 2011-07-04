#
# column indices in editarray, for every variable
editindex <- function(dm){
    S <- as.character(stack(dm)[,2])
    sapply(names(dm), function(n) which(n==S)) 
}

#' @nord
print.editarray <- function(x, ...){
    cat("editarray:\n")
    print(data.frame(getArr(x),n=getN(x)))
}

#' editarray: logical array where every column corresponds to one
#' level of one variable. Every row is an edit. Every edit denotes
#' a *forbidden* combination.
#' @nord
neweditarray <- function(E, ind, n=NULL, names=NULL, levels=colnames(E)){
    if ( is.null(names) ) names <- paste("e",1:nrow(E),sep="")
    if ( is.null(n) ) n <- rep(length(ind), nrow(E))
    dimnames(E) <- list(edits=names,levels=levels)
    structure(E,
        class  = "editarray",
        ind    = ind,
        n      = n
    )
}

# number of variables involved in the edits in E
nInvolved <- function(E){
    ind <- getInd(E)
    apply(E,1,function(e){
        sum(sapply(ind,
            function(I) if (sum(e[I]) < length(I))  1 else 0
        ))
    })
}


getVars.editarray <- function(E) names(attr(E,"ind"))

#' Generate a single edit restriction
#'
#'
#' @param ... edit restrictions in the form <var>=c("val1", "val2")
#' @param dm \code{\link{data.model}}
#' @param name name of edit.
#'
#' @return one-row \code{\link{editarray}} 
#' @nord
forbid <- function(dm, ..., name=NULL){

    L <- list(...)
    if ( !all(names(L) %in% names(dm)) )
        stop("edit contains variables not in data.model")
    
    d <- sum(dim(dm))
    e <- !array(logical(d),dim=c(1,d))
    colnames(e) <- c(dm,recursive=TRUE)
    rownames(e) <- name
    I <- editindex(dm) 
    for ( x in names(L) ){
        e[ 1, I[[x]][ !(dm[[x]] %in% L[[x]]) ] ] <- FALSE
    }

    neweditarray(e, ind = I, names=name, levels=c(dm,recursive=TRUE))
}


#' combine edit restrictions to an array
#'
#' @param ... objects of class \code{\link{editarray}}
#'
#' @nord
editarray <- function(...){
    neweditarray(rbind(...), ind = attr(..1,"ind") )
}

#' @nord
getInd <- function(E) attr(E,"ind")


#' @nord
getArr <- function(E) E[,,drop=FALSE]

#' @nord
getN <- function(E) attr(E,"n")

#' @nord
getlevels <- function(E) colnames(E)
#' @nord
getnames <- function(E) rownames(E)


# combine editarray by generating variable "var"
# intersection <-> and, union <-> or
combine <- function(E,var){ 
    ind <- getInd(E)
    lev <- getlevels(E)
    Arr <- getArr(E)
    I <- ind[[var]]
    n <- ncol(Arr)
    J <- 1:n
    J <- c(J[I],J[-I])
    e <- array(logical(n),dim=c(1,n))
    e[,J] <-  c(apply(Arr[, I],2,any), apply(Arr[, -I],2,all))
    neweditarray(e,ind, levels=lev)
}

# determine which edits in an editmatrix contain a certain variable.
contains <- function(E,var){
    I <- getInd(E)[[var]]
    V <- getArr(E)[,I,drop=FALSE]
    rowSums(V) < length(getInd(E)[[var]])
}

