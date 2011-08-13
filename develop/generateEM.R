# generate large editmatrix useful for performance testing


require(editrules)

genEditMatrix <- function(nedits, nvars, nvr=min(4, nvars)){
  nedits <- nedits
  nvars <- nvars
  gen <- function(){ 
    m <- matrix(0, nrow=nedits, ncol=nvars)
    for (i in 1:nedits){
      m[i,sample(nvars, nvr)] <- sample(c(-2,-1,1,2), size=nvr, replace=TRUE)
    }
    ops <- sample(c("==","<="), size=nedits, replace=TRUE)
    as.editmatrix(A=m, ops=ops)
  }
  
  E <- gen()
  while(!isFeasible(E)){
    E <- gen()
    nedits <- nedits-1
  }
  #decrease number of constraint until the matrix becomes feasible
  E
}

nvars <- 50
E <- genEditMatrix(10,nvars)
vars <- getVars(E)

x <- numeric(nvars) # since b==0, this is a valid solution
names(x) <- vars

x[sample(nvars,1)] <- 1

el <- errorLocalizer(E, x)
system.time({
  sol1 <- el$searchBest()
})
which(sol1$adapt)

system.time({
  sol2 <- localizeError_lp(E,x)
})
which(sol2$adapt)

#results should be analyzed!