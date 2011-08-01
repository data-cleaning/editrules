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

E <- genEditMatrix(20,100)
vars <- getVars(E)

x <- sample(10, size=100, replace=TRUE)
names(x) <- vars

el <- errorLocalizer(E, x)
#el$searchBest()
localizeError_lp(E,x)