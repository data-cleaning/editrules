#' determine which edits in an editmatrix contain a variable.
#'
#' For an \code{\link{editmatrix}}, variables with coefficients smaller than
#' \code{tol} are considered not to be contained in an edit.
#'
#' @param E \code{\link{editarray}}, \code{\link{editmatrix}}, \code{\link{editset}}, or \code{matrix}
#' @param var \code{character}, names of a categorical variables in \code{E}. If var=NULL, all variables are treated.
#' @param ... arguments to be passed to other methods
#' @return \code{logical} vector of length nrow(E), TRUE for edits containing \code{var}
#' @export
contains <- function(E,var=NULL,...){
    UseMethod('contains')
}


#' matrix method for contains
#' 
#' @method contains matrix
#' @rdname contains
#' @export
#' @keywords internal
contains.matrix <- function(E, var=NULL, tol=sqrt(.Machine$double.eps),...){
    if ( is.null(var) && !is.null(colnames(E)) ) var <- colnames(E)
    if ( is.null(var) ) var <- 1:ncol(E)
    u <- abs(E[,var,drop=FALSE]) > tol
    dimnames(u) <- dimnames(E[,var,drop=FALSE])
    u
}


#' editmatrix method for contains
#' 
#' @method contains editmatrix
#' @rdname contains
#' @param tol tolerance to check zero-entries
#' @export
#' @keywords internal
contains.editmatrix <- function(E, var=NULL, tol=sqrt(.Machine$double.eps), ...){
    A <- getA(E)
    if (is.null(var)) var <- getVars(E)
    
    u <- abs(A[,var,drop=FALSE]) > tol
    dimnames(u) <- list(edit=rownames(E),variable=var) 
    u
}

#' contains method for editarray
#'
#' @method contains editarray
#' @rdname contains
#' @export
#' @keywords internal
contains.editarray <- function(E,var=NULL,...){
    if ( !is.editarray(E) ) stop("Argument not of class editarray")
    ind <- getInd(E)
    if ( is.null(var)) var <- names(ind)
   
    contains.boolmat(getArr(E),ind,var)
}

#' determine if a boolean matrix contains var
#'
#' @param A array
#' @param ind index
#' @param var variable name
#' @keywords internal
contains.boolmat <- function(A, ind, var){
    ind <- ind[var]
    v <- vapply(ind, function(ii) rowSums(A[,ii,drop=FALSE]) < length(ii), FUN.VALUE=logical(nrow(A)))
    if ( is.vector(v) ){ 
        v <- array(v,dim=c(1,length(v)), dimnames=list(edit=rownames(A),var=names(v)))
    } else {
        dimnames(v) <- list(edit=rownames(A),variable=colnames(v))
    } 
  v
}


#' contains method for editset
#'
#' @method contains editset
#' @rdname contains 
#' @export
#' @keywords internal
contains.editset <- function(E,var=NULL,...){

    if ( is.null(var) ) var <- getVars(E)
    nedits <- nrow(E$num) + nrow(E$mixcat)
    # create a boolean array holding the answer
    T <- array(FALSE,
        dim=c(nedits,length(var)),
        dimnames=list(
            edits=c(
                rownames(E$num),
                rownames(E$mixcat)
            ),
            variables=var
        )
    )
    # contains for numerical variables
    numvar <- var[var %in% getVars(E$num)]
    nnum <- nrow(E$num)
    if ( length(numvar) > 0 && nnum > 0 )  T[1:nrow(E$num),numvar] <- contains(E$num, var[var%in% numvar])

    # contains for categorical variables in conditional edits (mix)
    nmix <- nrow(E$mixcat)
    if (nmix>0){
        imix <- (nnum+1):(nnum+nmix) 
        catvar <- var[var %in% getVars(E,type='cat')]
        T[imix,catvar] <- contains(E$mixcat, catvar) 
        # contains for numerical variables in mixed edits
        vnm <- var[var %in% getVars(E$mixnum)]
        vmc <- getVars(E$mixcat)
        emn <- rownames(E$mixnum)
        X <- contains(E$mixcat)[,vmc[vmc %in% emn],drop=FALSE]
        Y <- contains(E$mixnum,vnm)
        T[imix,vnm] <- X%*%Y >0
    }
    T
}


