
# derive implicit numerical edits by fourier-motzkin elimination.
implyNumericEdits <- function(E, var, assumeNormalized=TRUE){
    if (!assumeNormalized) E <- editmatrix(edits(E), normalize=TRUE)
    eps <- sqrt(.Machine$double.eps)
    Ivar <- abs(E[,var]) > eps

    vars <- getVars(E)
    jvar <- which(vars == var)

    E1 <- E[!Ivar,-jvar]

    F <- E[Ivar, ]    

    ops <- getOps(F)
    F <- F[order(getOps(F), decreasing=TRUE), ]
    nEq <- max(which(ops== "=="))
    nEdits <- length(ops)
 
    F <- cbind(as.matrix(F),getC(F))    

    # substitute equalities into (in)equalities
    A <- matrix(nrow=0, ncol=ncol(E),dimnames=list(NULL,c(vars[-jvar],"C"))) 

    v <- 1:min(nEq, nEdits-1)
    K <-  matrix(
        apply(F[v, -jvar,drop=FALSE],2,`/`,F[v, jvar,drop=FALSE]),
        nrow=length(v), dimnames=list(NULL, colnames(A)))

    
    op <- c()
    for ( k in v){
        I <- (k+1):nEdits
        e <- F[I, jvar,drop=FALSE]
        G <- F[I, -jvar, drop=FALSE]
        A <- rbind(A, sapply(1:ncol(G), function(j){
                    G[, j,drop=FALSE] - e * K[k, j] 
                }))
        op <- c(op, ops[I])
    }
    # Substitute inequalities in inequalities

    v <- (1:nEdits)[-v]    
    K <- matrix(
            apply(F[v,-jvar,drop=FALSE], 2, '/' ,abs(F[v,jvar])),
            nrow=length(v), dimnames=list(NULL, colnames(A)))
    
    w <- seq_along(v)
    for ( i in w[-length(w)] ){
        sigF <- sign(F[v[i],jvar])
        k <- K[i,,drop=FALSE]
        L <- lapply(w[w>i], function(j){
                if ( sigF != sign(F[v[j], jvar]) ) k + K[j,,drop=FALSE]
            })
        op <- c(op, ops[v[w>i & !sapply(L, is.null)]])
        A <- rbind(A,do.call(rbind,L))
    }
    #TODO split cases <, <= 
    #TODO reduce implicit edit matrix

    b <- c(A[ ,ncol(A)], getC(E1))
    as.editmatrix(rbind(A[,-ncol(A),drop=FALSE],getMatrix(E1)), C = b, ops=op) 

}

#E <- editmatrix(c(
#    "x + y -z == 1",
#    "2*x == y",
#    "2*z - x < u +2",
#    "x+y <= z"),
#    normalize=TRUE)
#
#implyNumericEdits(E,"x")

