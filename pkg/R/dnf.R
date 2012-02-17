
#' Disjunctive normal form for editsets
#'
#' An editset is transformed to a list of \code{\link{editset}s} 
#' which do not contain any mixed edits anymore. Each editset has an extra
#' attribute \code{condition}, which holds the series of assumptions made to 
#' decouple the original edits. This attribute will be printed when not \code{NULL}.
#'
#'
#' @param E an object of class \code{\link{editset}}
#' @return A list of editsets. Each has an attribute 'condition' showing which conditions 
#'  were assumed to derive the editset.
#' @example ../examples/dnf.R
#' @export
disjunct <- function(E){
    if (!inherits(E,'editset')) stop('only for objects of class editset')
    e <- new.env()
    dnf(E,"E", e)
    L <- as.list(e)
    names(L) <- NULL
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
        attr(E1,'condition') <- c(cnd,as.character(E$mixnum[vars[1]]))
        if ( isFeasible(E1$num) && isFeasible(E1$mixcat)) dnf(E1,nm[1],env)
        E2 <- substValue(E,vars[1],FALSE)
        attr(E2,'condition') <- c(cnd,invert(as.character(E$mixnum[vars[1]])))
        if (isFeasible(E2$num) && isFeasible(E2$mixcat)) dnf(E2,nm[2],env)
    }
}

