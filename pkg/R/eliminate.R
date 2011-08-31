eliminateFM <- function(E, var,...){
    warning("eliminateFM is deprecated. Use 'eliminate' in stead")
    eliminate(E,var,...)
}


#' Eliminate a variable from an editmatrix
#'
#' Uses Fourier-Motzkin elimination to eliminate a variable from a set
#' of linear (in)equality restrictions, represented as an \code{\link{editmatrix}}.
#' An observation of Kohler (1967) is used to reduce the number of implied 
#' restrictions. Obvious redundancies of the type 0 < 1 are removed as well. 
#'
#' @method eliminate editmatrix
#' @param E an object of class \code{\link{editmatrix}}
#' @param var \code{character} name of the variable to eliminate
#' @param fancynames \code{logical} If true, the derived restrictions have rownames derived from the original restrictions (slower).
#' @param ... other arguments to be passed to or from other methods
#' @return An editmatrix with an extra (hidden) attributes describing how the new restrictions were derived from the original ones.
#'    These attributes are used to remove redundancies when variables are repeatedly eliminated.
#' 
#' @example ../examples/eliminate.R
#'
#' @seealso \code{\link{editmatrix}} \code{\link{isObviouslyInfeasible}}
#' @references
#' D.A. Kohler (1967) Projections of convex polyhedral sets, Operational Research
#' Center Report , ORC 67-29, University of California, Berkely.
#' 
#' H.P. Williams (1986) Fourier's method of linear programming and its dual,
#' The American Mathematical Monthly 93, 681-695
#' @export
eliminate.editmatrix <- function(E, var, fancynames=FALSE, ...){
    if (!isNormalized(E)) E <- normalize(E)
    vars <- getVars(E)

    if (!var %in% vars){
        warning("Trying to eliminate variable not occuring in E")
        return(E)
    }
    d <- getH(E)   
    n <- geth(E)  
    if ( is.null(d) ){
        n <- 0
        d <- matrix(FALSE,nrow=nrow(E),ncol=nrow(E))
        diag(d) <- TRUE 
        colnames(d) <- rownames(E)
    }
    
    
    m <- as.matrix(E)
    ops <- getOps(E)
    
    coefs <- m[,var]
    I <- coefs != 0
    
    eq <- I & (ops == "==")
    
    upper <- which(!eq & coefs > 0)
    lower <- which(!eq & coefs < 0)
    
    coefs[!eq] <- abs(coefs[!eq])
    
    eq <- which(eq);

    # elimination possible?
    if ( (length(upper) > 0 && length(lower) > 0) ||
         (length(eq) >= 1 && (length(upper) > 0 || length(lower) > 0)) ||
         (length(eq) >= 2) ){
       n <- n+1
    } else {
        return(E[ E[ ,var]==0,])
    } 


    #normalize matrix, every row has coefficient 1 or -1 for var
    m[I,] <- m[I,] / coefs[I]

    # eqs and ineqs w/coef>0 ==> ineqs w/coef<0
    equpper <- c(eq, upper)
    I1 <- rep(equpper,each=length(lower))
    I2 <- rep(lower, times=length(equpper))
    ml <- m[I1,,drop=FALSE] + m[I2,,drop=FALSE]
    ol <- ifelse(ops[I1] != "<", ops[I2], ops[I1])
    dl <- d[I1,,drop=FALSE] | d[I2,,drop=FALSE]

    # eqs ==> ineqs w/coef>0
    I1 <- rep(eq,each=length(upper))
    I2 <- rep(upper, times=length(eq))
    mu <- m[I2,,drop=FALSE] - m[I1,,drop=FALSE]
    ou <- ops[I2]
    du <- d[I1,,drop=FALSE] | d[I2,,drop=FALSE]

    # eqs ==> eqs
    me <- m[logical(0),,drop=FALSE]
    de <- d[logical(0),,drop=FALSE]
    if ( length(eq)>1){
        me <- t(t(m[eq[-1],,drop=FALSE]) - m[eq[1],])
        de <- t(t(d[eq[-1],,drop=FALSE]) | d[eq[1],])       
    } 
    oe <- rep("==",nrow(me))

    m <- rbind(ml,mu,me,m[!I,,drop=FALSE])
    d <- rbind(dl,du,de,d[!I,,drop=FALSE])
    o <- c(ol,ou,oe,ops[!I])
    redundant <- rowSums(d) > n + 1 | isObviouslyRedundant.matrix(E=m, operators=o)

    m <- m[!redundant,,drop=FALSE]
    d <- d[!redundant,,drop=FALSE]
    if ( nrow(m) > 0 ){
        if ( fancynames ){
            rownames(m) <- paste("e",sapply(lapply(1:nrow(d),function(i) which(d[i,]) ),paste,collapse="."),sep="")
        } else {
            rownames(m) <- paste("e",1:nrow(m),sep="")
        }
    }
    neweditmatrix(
          m
        , o[!redundant]
        , normalized = TRUE 
        , H = d 
        , h = n
    )
}





