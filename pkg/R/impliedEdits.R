
# derive implicit numerical edits by fourier-motzkin elimination.
implyNumericEdits <- function(E, var, assumeNormalized=TRUE){
    ops <- getOps(E)
    E <- E[order(getOps(E), decreasing=TRUE), ]
    nEq <- max(which(ops== "=="))
    nEdits <- length(ops)
 
    ivar <- which(getVars(E) == var)
    K <-  apply(E[ , -ivar],2,`/`,E[ , ivar])
    C <- getC(E)
    v =  C/E[,ivar]
    
    A <- matrix(nrow=0,ncol=ncol(E)-1)
    b <- c()
    # substitute equalities into (in)equalities
    for ( k in 1:min(nEq,nEdits-1)){
        I <- (k+1):nrow(K)
        G <- as.matrix(E[I, -ivar])
        A <- rbind(A, sapply(1:ncol(G), function(j){
                    G[, j,drop=FALSE] - E[I, ivar,drop=FALSE]*K[k, j,drop=FALSE] 
                }))
        b <- c(b, C[I] - v[k]*E[I,ivar] )
    }
    # need to add substitution of inequalities into inequalities
    colnames(A) <- colnames(E[,-ivar]) 
    as.editmatrix(A, C= b, ops=ops)
}


E <- editmatrix(c(
    "x + y -z == 1",
    "2*x == y"),
    normalize=TRUE)

implyNumericEdits(E,"x")

