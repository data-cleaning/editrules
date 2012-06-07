
#' Remove redundant variables and edits.
#'
#' Remove variables which are not contained in any edit and remove edits which are 
#' \code{\link[=isObviouslyRedundant]{obviously redundant}}.
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param ... arguments to pass to other methods
#'
#' @export
#' @seealso \code{\link{contains}}, \code{\link{eliminate}}, \code{\link{substValue}}
#'
reduce <- function(E,...){
    UseMethod('reduce')
}


#' 
#' @method reduce editmatrix
#' @param tol elements of \code{E} with absolute value < \code{tol} are considered 0.
#' 
#' @rdname reduce
#' @export
reduce.editmatrix  <- function(E, tol=sqrt(.Machine$double.eps),...){
  if ( nrow(E) == 0 ) return(neweditmatrix(matrix(numeric(0)),character(0)))
  m <- as.matrix(E)
  if ( tol > 0 ) m[abs(m) < tol] <- 0
  B <- m != 0
  v <- 1:(ncol(m)-1)
  vars <- which(colSums(B[,v,drop=FALSE]) != 0)
  edits <- (rowSums(B) != 0)
  E[edits,c(vars,ncol(m)) , drop=FALSE]
}


#'
#' @method reduce editarray
#' 
#' @export
#' @rdname reduce
reduce.editarray <- function(E,...){
    E <- E[!isObviouslyRedundant.editarray(E),,drop=FALSE]
    m <- as.matrix(E)
    ind <- getInd(E)
    H <- getH(E)
    b <- sapply(ind,function(ind) all(m[,ind,drop=FALSE])) 
    if ( any(b) ){
        J <- logical(0)   
        for ( j in ind[b] ) J <- c(J,j)
        sep=getSep(E)
        m <- m[,-J,drop=FALSE]
        ind <- indFromArray(m,sep=sep)
        i <- apply(!m,1,all)
        m <- m[!i,,drop=FALSE]
        if (!is.null(H))  H <- H[!i,,drop=FALSE]
        E <- neweditarray(E=m, ind=ind, sep=sep, names=rownames(m), H=H)
    }
    E
}


#'
#' @method reduce editset
#' @export
#' @rdname reduce
#'
reduce.editset <- function(E,...){

    num <- reduce(E$num)
    mixcat <- reduce(E$mixcat)
    v <- getVars(mixcat)
    mixnum <- reduce(E$mixnum[rownames(E$mixnum) %in% v,])

    imix <- grepl("^.num",v)
    if ( nrow(mixcat) > 0 ){
        m <- logical(nedits(mixcat)) 
        if ( any(imix) ) m <- apply(contains(mixcat, v[imix]), 1, any)
        pref <- ifelse(m,"mix","cat")
        rownames(mixcat) <- paste(pref, 1:nrow(mixcat), sep="")
    }
    simplify(neweditset(
        num = num,
        mixnum = mixnum,
        mixcat = mixcat
    ))

}



