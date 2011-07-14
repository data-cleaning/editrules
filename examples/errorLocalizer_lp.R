require(editrules)

#' extends an editmatrix with extra constraint useful for error
#' localization
#' @param E editmatrix 
#' @param x named numeric with data
#' @return list with extended E, objfn and lower and upper bound
buildELMatrix <- function(E,x, weight=rep(1, length(x))){
  vars <- getVars(E)
  x <- x[vars]
  weight <- weight[match(vars, names(x))]
  
  nvars <- length(vars)
  
  #TODO cope with NA's in x
  binvars <- paste("adapt", vars, sep=".")
  binidx <- seq_along(vars) + nvars
  
  lb=rep(0, nvars)
  ub = 1000 * x #heuristic
  
  A <- getA(E)
  
  Ael <- cbind(A,A,getb(E))
  colnames(Ael)[binidx] <- binvars
  Ael[,binidx] <- 0
       
  r_x <- diag(-1, nvars)
  r_u <- diag(lb-x)
  r <- cbind(r_x, r_u, -x)
  Ael <- rbind(Ael, r)
   
  r_x <- diag(1, nvars)
  r_u <- diag(x-ub)
  r <- cbind(r_x, r_u, x)
  Ael <- rbind(Ael, r)
    
  ops <- c(getOps(E), rep("<=", 2*nvars))
  
  nb <- ncol(Ael)
  Eel <- as.editmatrix( Ael[,-nb,drop=FALSE]
                      , Ael[,nb]
                      , ops
                      )
  
  objfn <- numeric(2*nvars)
  objfn[binidx] <- weight
  
  names(lb) <- names(ub) <- vars
  names(objfn) <- getVars(Eel)
  
  list( E = Eel
      , objfn = objfn
      , lb=lb
      , ub=ub
      )
}


errorLocalize_glpk <- function(E, x, weight=rep(1, length(x))){
   vars <- getVars(E)
   elm <- buildELMatrix(E,x, weight)
   types <- ifelse(elm$objfn, "B", "C")
   E <- elm$E
   binidx <- which(elm$objfn > 0)

   if (!require(Rglpk)){
      stop("Install 'Rglpk' to use this function")
   }
   sol <- Rglpk_solve_LP( obj=elm$objfn
                 , mat=getA(E)
                 , dir=getOps(E)
                 , rhs=getb(E)
                 , types=types
                 , max=FALSE
                 , verbose=FALSE
#                 , bounds=list( lower=list(ind=seq_along(vars), val=lb)
#                              , upper=list(ind=seq_along(vars), val=ub)
#                              )
                )
   names(sol$solution) <- c(vars,vars) 
   list( w=sol$optimum
       , adapt = (sol$solution > 0)[binidx]
       , x_c = sol$solution[-binidx]
       )
}

errorLocalize_lp <- function(E, x, weight=rep(1, length(x)), verbose="neutral"){
   vars <- getVars(E)
   elm <- buildELMatrix(E, x, weight)
   E <- elm$E
   objfn <- elm$objfn
   binidx <- which(objfn > 0)
   
   ops <- getOps(E)
   ops[ops=="=="] <- "="
 
   if (!require(lpSolveAPI)){
      stop("Install 'lpSolveApi' to use this function")
   }
   
   A <- getA(E)
   lps <- make.lp(nrow(A), ncol(A), verbose=verbose)
   dimnames(lps) <- dimnames(A)
   for (v in 1:ncol(A)){
     set.column(lps, v, A[,v])
   }
   set.constr.type(lps,types=ops)
   set.constr.value(lps, getb(E))
   set.bounds(lps, lower=elm$lb, upper=elm$ub, columns=1:length(elm$lb))
   set.type(lps, columns=binidx , "binary")
   set.objfn(lps, objfn)

   solve(lps)
   
   sol <- get.variables(lps)#[binidx]
   w <- get.objective(lps)
   #write.lp(lps, "test.lp")
 
   delete.lp(lps)
   
   names(sol) <- c(vars,vars)
   list( w=w
       , adapt=(sol > 0)[binidx]
       , x_c = sol[-binidx]
       )
}

   
   
   

#testing...
Et <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0")
               )

x <- c(p=755,c=125,t=200)

errorLocalize_lp(Et, x)
errorLocalize_glpk(Et, x)
