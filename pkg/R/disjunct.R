
#' Decouple a set of conditional edits
#'
#' An editset is transformed to a list of \code{\link{editset}s} 
#' which do not contain any conditional numeric/categorical edits anymore. Each editset has an extra
#' attribute \code{condition}, which holds the series of assumptions made to 
#' decouple the original edits. This attribute will be printed when not \code{NULL}.
#'
#' At the moment this functionality is somewhat experimental and in- or output specification
#' should be expected to change in coming releases.
#'
#' @param E an object of class \code{\link{editset}}
#' @return An object of class \code{editlist}, which is nothing more than a \code{list} of \code{editsets} with a class attribute.
#'  Each element has an attribute 'condition' showing which conditions were assumed to derive the editset.
#'
#' @example ../examples/dnf.R
#' @export
disjunct <- function(E){
    if (!inherits(E,'editset')) stop('only for objects of class editset')
    e <- new.env()
    dnf(E,"E", e)
    L <- as.list(e)
    names(L) <- NULL
    class(L) <- c("editlist")
    L
}

dnf <- function(E, name, env){
    cnd <- attr(E,'condition')
    vars <- getVars(E,type="dummy")
    if (length(vars) == 0){ 
        assign(name,E,envir=env)
    } else {
        nm <- paste(vars[1],c("T","F"),sep="_")
        nm <- paste(name,nm,sep="_and_")
        E1 <- substValue(E,vars[1],TRUE)
        if ( isFeasible(E1$num) && isFeasible(E1$mixcat)) dnf(E1,nm[1],env)
        E2 <- substValue(E,vars[1],FALSE)
        if (isFeasible(E2$num) && isFeasible(E2$mixcat)) dnf(E2,nm[2],env)
    }
}



#' Separate an editset into its disconnected blocks and simplify
#'
#' @param E an editset
#' @return A \code{list} where each element is either an \code{\link{editmatrix}}, an \code{\link{editarray}}
#' or an object of class \code{editsets}
#' @keywords internal
#' @example ../examples/separate
separate <- function(E){
    B <- blocks(E)
    B <- lapply(B, function(b){
        et <- editType(b)
        if ( all(et == 'num') ){ 
            b <- b$num
        } else if ( all(et == 'cat') ){
            b <- b$mixcat
        } else {
            b <- disjunct(b)
        }
        b
    })
}











