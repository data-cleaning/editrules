#'  
#'
#'
#'
#'
#'
#'
#' 
eliminator <- function(E, var, assumeNormalized=TRUE){
    if (!assumeNormalized) E <- normalize(E)
    vars <- getVars(E)
    
    if (!var %in% vars){ # nothing to eliminate...
        return(E)
    }
    
    m <- as.matrix(E)
    ops <- getOps(E)
    C <- getb(E)
    
    coefs <- m[,var]
    I <- coefs != 0
    
    eq <- I & (ops == "==")
    
    upper <- which(!eq & coefs > 0)
    lower <- which(!eq & coefs < 0)
    
    coefs[!eq] <- abs(coefs[!eq])
    
    eq <- which(eq);
    #normalize matrix, every row has coefficient 1 or -1 for var
    m[I,] <- m[I,] / coefs[I]

    # eqs and ineqs w/coef>0 ==> ineqs w/coef<0
    equpper <- c(eq, upper)
    I1 <- rep(equpper,each=length(lower))
    I2 <- rep(lower, times=length(upper))
    ml <- m[I1,] + m[I2,]
    ol <- ifelse(ops[I1] != "<", ops[I2], ops[I1])

    # eqs ==> ineqs w/coef>0
    I1 <- rep(eq,each=length(upper))
    I2 <- rep(upper, times=length(eq))
    mu <- m[I2,] - m[I1,]
    ou <- ops[I2]

    # eqs ==> eqs
    me <- m[logical(0),,drop=FALSE]
    if ( length(eq)>0)
        me <- t(t(m[eq[-1],,drop=FALSE]) - m[eq[1],]) 
    oe <- rep("==",nrow(me))

    m <- rbind(ml,mu,me,m[!I,,drop=FALSE])

    as.editmatrix(m[,-ncol(m),drop=FALSE], m[,ncol(m)], c(ol,ou,oe,ops[!I]))
}

