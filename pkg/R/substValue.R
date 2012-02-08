#' Replace a variable by a value in a set of edits.
#'
#' @param E \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param var \code{character} with name(s) of variable(s) to substitute
#' @param value vector with value(s) of variable(s)
#' @param ... arguments to be passed to or from other methods
#' @return \code{E}, with variables replaced by values
#' @example ../examples/substValue.R
#' @seealso \code{\link{eliminate}}
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
#' @param reduce \code{logical} should substituted variables  be removed?
#' @param removeredundant \code{logical} should empty rows be removed? 
#"
#' @rdname substValue 
#' @export
substValue.editmatrix <- function(E, var, value, reduce=FALSE, removeredundant=TRUE, ...){
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
    if (removeredundant) {
        return( E[!isObviouslyRedundant.editmatrix(E),] )
    } else {
        return(E)
    }  
}



#' Substitute a value in an editarray
#'
#' For editarrays, only rows with \code{<var>:<value>==TRUE} are kept.
#' In the kept rows, categories not equal to <value> are set to \code{FALSE}
#' If \code{reduce=TRUE}, columns corresponding to categories which are set
#' to \code{FALSE} will be removed. Note that the function \code{\link{reduce}}
#' has a different effect (it removes complete variables).
#'
#' @method substValue editarray
#'
#'
#' @rdname substValue
#'
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



#' Substitute values in an \code{\link{editset}}
#'
#' For an \code{\link{editset}}, purely numerical variables are
#' substitutes as in an \code{\link{editmatrix}} and categorical
#' as in an \code{\link{editarray}}. Numerical variables appearing
#' logical constraints are substituted and if truth values can
#' be derived these are substituted in the logical constraint.
#' 
#' @param simplify Simplify editset by moving logical edits containing a simple 
#'      numerical statement to the pure numerical part?
#'
#' @method substValue editset
#'
#' @rdname substValue 
#' @export
substValue.editset <- function(E, var, value, simplify=TRUE, ...){
    # the nonnumeric case is simple
    if ( !is.numeric(value) ){
        E$mixcat <- substValue(E$mixcat,var,value,...)
        return(E)
    }
    # substitute pure numeric data
    i1 <- var %in% getVars(E$num)
    if ( any(i1) ){ # time-saving condition
        numvar <- var[i1]
        numval <- value[i1]
        innum <- colSums(contains(E$num, numvar )) > 0
        if ( any(innum) ) 
            E$num <- substValue(E$num, numvar[innum], numval[innum])
    }
    i1 <- var %in% getVars(E$mixnum)
    if ( any (i1) ){ # time-saving condition
        mixvar <- var[i1]
        mixval <- value[i1]
        u <- contains(E$mixnum, mixvar)
        inmix <- colSums(u) > 0
        if ( any(inmix) ){
            E$mixnum <- substValue(
                E$mixnum, 
                mixvar[inmix], 
                mixval[inmix],
                removeredundant=FALSE
            )
            # did substitution yield any certainties?
            cntr <- isContradiction(E$mixnum)
            taut <- isTautology(E$mixnum)
            # dummy variables to be eliminated from mixcat
            lvar <- apply(u[,inmix,drop=FALSE],1,any)
            dvar <- rownames(u)
            dval <- logical(length(taut))
            dval[lvar & cntr] <- FALSE
            dval[lvar & taut] <- TRUE
            isub <- lvar & (cntr | taut)
            E$mixcat <- substValue(E$mixcat, dvar[isub],dval[isub])
        }
    }
    if ( simplify ) E <- simplify(E)
    E
}




# Returns which linear edits are obvious contradictions.
#  - Accurate to 8 figures.
#  - Assumes editmatrix normality
isContradiction <- function(E){
    tol = 1e-8
    ops <- getOps(E)
    absA <- abs(getA(E))
    nil <- rowSums(absA) < ncol(absA)*tol
    b <- getb(E)
    I <- logical(nrow(absA))
    eq <- ops=='=='
    lt <- ops=='<'
    le <- !eq & !lt
    I[eq] <- nil[eq] & abs(b[eq]) > tol
    I[lt] <- nil[lt] & b[lt] <= -tol 
    I[le] <- nil[le] & b[le] < tol
    I
}

# returns which linear edits are obviously TRUE
# - Accurate to 8 figures
# - Assumes editmatrix normality
isTautology <- function(E, tol=sqrt(.Machine$double.eps)){
    tol = 1e-8
    ops <- getOps(E)
    absA <- abs(getA(E))
    nil <- rowSums(absA) < ncol(absA)*tol
    b <- getb(E)
    I <- logical(nrow(absA))
    eq <- ops=='=='
    lt <- ops=='<'
    le <- !eq & !lt
    I[eq] <- nil[eq] & abs(b[eq]) < tol
    I[lt] <- nil[lt] & b[lt] > tol
    I[le] <- nil[le] & b[le] >= -tol
    I
}












