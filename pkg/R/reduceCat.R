#' eliminate one category from a logical array, not for export.
#' TODO redundancy removal by recording derivation history.
#' @nord
eliminateCat <- function(A, J, j, H, h){
    j1 <- A[,J[j]]
    j2 <- !j1
    n1 <- sum(j1)
    n2 <- sum(j2)
    if (n1==0 || n2==0) return(A)
    I1 <- rep(which(j1), times=n2)
    I2 <- rep(which(j2), each=n1)
    B <- array(FALSE,dim=c(n1*n2,ncol(A)))
    B[,J] <- A[I1,J,drop=FALSE] | A[I2,J,drop=FALSE]
    B[,-J] <- A[I1,-J,drop=FALSE] & A[I2,-J,drop=FALSE]
    A <- rbind(A[j1,,drop=FALSE],B)
    # detect redundancies and get rid of them
    H <- rbind(H[j1,,drop=FALSE],H[I1,,drop=FALSE] | H[I2,,drop=FALSE])
    I3 <- rowSums(H) > h+1
    list(A=A[!I3,,drop=FALSE], H=H[I3,,drop=FALSE], h=h+1)
}

#' Eliminate variable from editarray.
#' 
#' @method eliminate editarray
#' @rdname eliminate
#' @export
eliminate.editarray <- function(E, var, ...){
    J <- getInd(E)[[var]]
    sep <- getSep(E) 
    A <- getArr(E)
    h <- geth(E)
    if ( is.null(h) ){
        h <- 0
        H <- matrix(FALSE,ncol=nrow(E),nrow=nrow(E))
        diag(H) <- TRUE
    } else {
        H <- getH(E)
    }
    for ( j in 1:length(J)){
         red <- duplicated(A) | isObviouslyRedundant(A)
         L <- eliminateCat(A[!red,,drop=FALSE],J,j)
         A <- L$A
         H <- L$H
         h <- L$h
    }
    neweditarray(E=A, ind=getInd(E), sep=sep, levels=getlevels(E), H=H, h=h)
}

#' duplicated method for editarray
#' @method duplicated editarray
#' @param x a \code{\link{editarray}}
#' @param ... other parameters to be passed to or from other methods.
#' @export
duplicated.editarray <- function(x, ...) duplicated(getArr(x))


#' check if any edit rule is a subset of another one)
#'
#' @param E editarray
#' @return \code{logical} vector
#' @export
isSubset <- function(E){
    E <- getArr(E)
    # TODO: is this any faster with a while loop? Should we do this in C?
    m <- nrow(E)
    if ( m == 0 ) return(logical(0))
    m1 <- m-1
    sapply(1:m, function(i){
        any(rowSums(E[-i,,drop=FALSE] - (E[rep(i,m1),,drop=FALSE] | E[-i,,drop=FALSE])) == 0)
    })
}

#' Check for obvious infeasibility
#'
#' Check if boolean rep. of an edit is TRUE for every variable and every category?
#' @param E editarray
#' @param ... Other arguments to be passed to or from other methods
#' @export
isObviouslyInfeasible.editarray <- function(E,...){
    any(rowSums(E)==ncol(E))
}


#' Substitute a value in an editarray: 
#'
#' @param E an editarray
#' @param var \code{character}, the variable name
#' @param value \code{character}, the value
#' @param ... other arguments to be passed to or from other methods
#' @return \code{\link{editarray}} with one variable substituted
#'
#' @export
substValue.editarray <- function(E, var, value, ...){
# TODO: make this work for multiple variables and values.
    J <- getInd(E)[[var]]
    sep=getSep(E)
    value <- paste(var,value,sep=sep)
    ival <- intersect(which(colnames(E) == value), J) 
    if ( length(ival) != 1 ) 
        stop(paste("Variable ", var,"not present in editarray or cannot take value",value))
    ii <- setdiff(J,ival)
    A <- getArr(E)
    A[,ii] <- FALSE
    I <- A[,ival]
    neweditarray(E=A[I,,drop=FALSE], ind=getInd(E), sep=sep, levels=getlevels(E))
}

#' check if any of the variables has FALSE for every category (a record can never be contained in such a set)
#' @method isObviouslyRedundant editarray
#' @param E \code{editarray} 
#' @param ... other arguments to be passed to or from other methods.
#' @export
isObviouslyRedundant.editarray <- function(E,...){
    ind <- getInd(E)
    for ( I in ind ) if ( any(apply(!E[,I,drop=FALSE],1,all)) ) return(TRUE)
    return(FALSE)
}

