
# derive implicit numerical edits by fourier-motzkin elimination.
implyNumericEdits <- function(E, var){
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
print(k)
        I <- (k+1):nrow(K)
print(I)
        G <- as.matrix(E[I, -ivar])
print(G)
        A <- rbind(A, sapply(1:ncol(G), function(j){
                    G[, j,drop=FALSE] - E[I, ivar,drop=FALSE]*K[k, j,drop=FALSE] 
                }))
print(A)
        b <- c(b, C[I] - v[k]*E[I,ivar] )
print(b)
    }

    colnames(A) <- colnames(E[,-ivar]) 

    s<-as.editmatrix(A, C= b, ops=ops)
print(s)
return(s)
}


#E <- editmatrix(c(
#    "x + y -z == 1",
#    "2*x == y"),
#    normalize=TRUE)
#
#implyNumericEdits(E,"x")

