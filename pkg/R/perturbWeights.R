



# add perturbations to weights, without changing the sort order of nonunique 
# elements
#
#
perturbWeights <- function(x){
    idup <- duplicated(x)
    ndup = sum(idup)
    nx <- length(x)
    if ( ndup == 0 ) return(x)
    if ( ndup == (nx-1) ) return(runif(nx,0.5,1))
    y <- sort(x)
    d <- abs(diff(y))
    
    e <- min(d[d>0])/100
    p <- runif(sum(idup),-e,e)
    x[idup] <- abs(x[idup] + p)
    x
}

