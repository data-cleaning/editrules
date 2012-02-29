
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
#' @param E Object of class \code{\link{editset}}
#' @param type Return type: \code{list} (default) for \code{editlist}, \code{env} for \code{editenv}.
#' @return An object of class \code{editlist} (\code{editenv}), which is nothing more than a \code{list} (\code{environment}) of 
#'  \code{editsets} with a class attribute. Each element has an attribute 'condition' showing which conditions 
#'  were assumed to derive the editset.
#'
#' @example ../examples/dnf.R
#' @export
disjunct <- function(E, type=c('list','env')){
    if (!inherits(E,'editset')) stop('only for objects of class editset')
    type=match.arg(type)
    e <- new.env()
    e$i <- 0
    dnf(E, e)
    rm('i',envir=e)
    if ( type == 'list'){
        e <- as.list(e)
        class(e) <- c("editlist")
    } else {
        class(e) <- c("editenv")
    }
    e
}

dnf <- function(E, env){
    cnd <- attr(E,'condition')
    vars <- getVars(E,type="dummy")
    if (length(vars) == 0 ){
            env$i <- env$i + 1
            assign(paste("E",env$i,sep=""),E,envir=env)
    } else {
        E1 <- substValue(E,vars[1],TRUE)
        if ( isFeasible(E1$num) && isFeasible(E1$mixcat)) dnf(E1,env)
        E2 <- substValue(E,vars[1],FALSE)
        if (isFeasible(E2$num) && isFeasible(E2$mixcat)) dnf(E2,env)
    }
}



#' Separate an editset into its disconnected blocks and simplify
#'
#' @param E an editset
#' @return A \code{list} where each element is either an \code{\link{editmatrix}}, an \code{\link{editarray}}
#' or an object of class \code{\link[=disjunct]{editlist}} which cannot be simplified further.
#' 
#' @example ../examples/separate.R
#' @export
#' @seealso \code{\link{blocks}}
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
    B
}


# For an editlist, determine the variable type and where in the list it occurs
# E editlist
# var character
# returns
# list()
#   $type   : 'num' or 'mixcat'
#   $occurs : logical of length(E)
varTypeAndOccurrence <- function(E,var){
    ivar <- sapply(E,function(e) c(
            var %in% getVars(e$num),
            var %in% getVars(e$mixcat)
        )
    )
    if ( sum(ivar) == 0 ){
        return(NA)
    }
    if ( !(is.array(ivar)) ) ivar <- array(ivar,dim=c(2,1))
    if ( any(ivar[1,]) ) {
        type <- 'num'
        iset <- ivar[1,]
    } else {
        type <- 'mixcat'
        iset <- ivar[2,]
    }
    list(type=type,occurs=iset)
}











