
source_dir <- function(d){
   stopifnot(file.exists(d))
   fs <- dir(d,full.names=TRUE)
   for ( f in fs ) source(f)
}


source_dir("../pkg/R")

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
S <- smoke_test(100,nvar=7,2, distr=rnorm)
all(S[[1]]$status$weight == S[[2]]$status$weight)
i <- which(S[[1]]$status$weight != S[[2]]$status$weight)

d <- S[[3]][1,]
e <- S[[4]]
load("test.Rdata")

save(e,d,file="test.Rdata")



rec <- do.call(c,S[[3]])
errorLocalizer.mip(S[[4]],rec)
errorLocalizer(S[[4]],rec)$searchBest()

localizeErrors(S[[4]],S[[3]])

source_dir("../editrules/pkg/R")
localizeErrors(S[[4]],S[[3]],method='mip')



write.csv(x,file="testrecord.csv")
write(as.character(S[[4]]),file="testedits.txt")

e <- editmatrix(expression(x + y == z, x>0,y>0,z>0,w>0))
d <- data.frame(x = -1, y=2,z=4,w=-1)
localizeErrors(e,d,method='mip',verbose=TRUE)





