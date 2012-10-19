
# n : number of variables
# m : number of blocks (< (n-3)/2)
accountBalance <- function(n, m, all.positive=TRUE){
   nblock <- (n-m-1) %/% m
   varnames <- paste("x",1:n,sep="")
   A <- matrix(0, nrow=(m+1), ncol=n)
   for ( i in 1:m ){
      iblock = ((i-1)*nblock+1):(i*nblock)
      A[i,iblock] <- 1
      A[i,(i*nblock)+1] <- -1
   } 
   A[m+1,seq(nblock+1,n-m,by=nblock)] <- 1
   A[m+1,n] <- -1
   
   pos = character(0)
   if ( all.positive ) pos <- paste(varnames, 0, sep=">")
   c(
      reduce(as.editmatrix(A)),
      editmatrix(pos)
   )
}

# Generate N records for variables E.
gen_data <- function(E,N,distr=rlnorm,...){
   vars <- getVars(E)
   nval <- N*length(vars)
   as.data.frame(
      array(
         distr(nval,...),
         dim=c(N,length(vars)), 
         dimnames=list(NULL, vars)
      )
    )
}



smoke_test <-function(N, nvar, nblocks, all.positive=TRUE, ...){
   e <- accountBalance(nvar, nblocks,all.positive)
   dat <- gen_data(e,N,...)
   el1 <- localizeErrors(e,dat,verbose=TRUE)
   el2 <- localizeErrors(e,dat,verbose=TRUE,method="mip")
   list(el1,el2,dat,e)
}

## this shows that even a well-scaled problem may give different results between
## B&B and MIP 
S <- smoke_test(100,nvar=7,2,distr=rnorm)
diff <- S[[1]]$status$weight == S[[2]]$status$weight

if ( !all(diff)) {
  w <- which(!diff)
  
  BB <- S[[1]]$adapt[w,]
  MIP <- S[[2]]$adapt[w,]
  dat <- S[[3]][w,]
  E <- S[[4]]
  
  x <- dat[1,,drop=TRUE]
  el <- errorLocalizer.mip(E,x)
  
  cbind(el$x_feasible, x, el$adapt)

}