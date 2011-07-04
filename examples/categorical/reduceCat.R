# eliminate one category from a logical array, not for export.
# TODO redundancy removal by recording derivation history.
eliminateCat <- function(A, n, J, j){
    j1 <- A[,J[j]]
    j2 <- !j1
    n1 <- sum(j1)
    n2 <- sum(j2)
    if (n1==0 || n2==0) return(list(A=A,n=n))
    I1 <- rep(which(j1), times=n2)
    I2 <- rep(which(j2), each=n1)
    B <- array(FALSE,dim=c(n1*n2,ncol(A)))
    B[,J] <- A[I1,J,drop=FALSE] | A[I2,J,drop=FALSE]
    B[,-J] <- A[I1,-J,drop=FALSE] & A[I2,-J,drop=FALSE]
    print(B)
    print(pmax(n[I1],n[I2]))
    list(A=B, n=pmax(n[I1],n[I2]))
}

# TODO  1. prove that this works --DONE 23.05.2011
#       2. redundancy removal    --DONE. 23.05.2011
#       3. robustness for empty arrays etc. --Needs testing
#       4. history recording for redundancy removal
eliminateFM.editarray <- function(E, var){
    J <- getInd(E)[[var]]
    A <- getArr(E)
    n <- getN(E)
    for ( j in 1:length(J)){
         red <- duplicated(A) | isObviouslyRedundant.array(A)
         L <- eliminateCat(A[!red,,drop=FALSE],n[!red],J,j)
         A <- L$A
         n <- L$n
    }
    neweditarray(E=A, ind=getInd(E),n=n, levels=getlevels(E))
}

# duplicated method for editarray
duplicated.editarray <- function(x, ...) duplicated(getArr(E))

# redundancy check - editarray method.
isObviouslyRedundant.editarray <- function(E, ...){
    isObviouslyRedundant.array(getArr(E))
}

# redundancy check (check if any edit rule is a subset of another one)
isObviouslyRedundant.array <- function(E, ...){
    # TODO: is this any faster with a while loop? Should we do this in C?
    m <- nrow(E)
    m1 <- m-1
    sapply(1:m, function(i){
        any(rowSums(E[-i,,drop=FALSE] - (E[rep(i,m1),,drop=FALSE] | E[-i,,drop=FALSE])) == 0)
    })
}


# replace a value in an editarray: 
#   remove rows of E for which have var[value] == FALSE
#   set levels of var[!value] to FALSE
substValue.editarray <- function(E, var, value){
    J <- getInd(E)[[var]]
    ival <- intersect(which(colnames(E) == value), J) 
    if ( length(ival) != 1 ) 
        stop(paste("Variable ", var,"not present in editarray or cannot take value",value))
    ii <- setdiff(J,ival)
    A <- getArr(E)
    n <- getN(E) - A[,ival]     
    A[,ii] <- FALSE
#    A[,J] <- TRUE
    I <- A[,ival]
    neweditarray(E=A[I,,drop=FALSE], ind=getInd(E), n=n[I], levels=getlevels(E))
}

# isObviouslyInfeasible should be lifted to S3 generic.
# check if any of the variables has FALSE for every category.
isObviouslyInfeasible.editarray <- function(E){
    ind <- getInd(E)
    for ( I in ind ) if ( any(apply(!E[,I,drop=FALSE],1,all)) ) return(TRUE)
    return(FALSE)
}

