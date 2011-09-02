# Error localization formulated as a mixed integer programming problem
# Advantage: can be very fast for localization problems with hundreds to thousands 
# of variables
# Disadvantage:
#           * gives only one solution out of possibly multiple
#           * numeric instability may give no, or false solutions, how ever these can
# be checked with editrules isFeasible functionality. Furthermore, numeric instability can be decreased by using appropiate upper
# and lower bounds
#
# TODO 
# * support for NA
# * 

#' Extends an editmatrix with extra constraints needed for error
#' localization
#' @param E editmatrix 
#' @param x named numeric with data
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix <- function(E,x, weight=rep(1, length(x))){
  vars <- getVars(E)
  x <- x[vars]
  weight <- weight[match(vars, names(x))]
  
  nvars <- length(vars)
  
  #TODO cope with NA's in x (choose upper en lower bound and  don't generate error localization contraints for na's')
  binvars <- paste("adapt", vars, sep=".")
  binidx <- seq_along(vars) + nvars
  
  #TODO get upperbounds and lowerbounds present in E and put them in the bounds
  # assume that boundary for each variable is at most 1000 times higher than given value 
  ub = 1000 * abs(x) #heuristic
  ub[is.na(ub)] <- 1000
  ub[ub < 1000] <- 1000
  lb <- -ub
  
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

# localizeError_glpk <- function(E, x, weight=rep(1, length(x)), verbose=FALSE){
#    vars <- getVars(E)
#    elm <- buildELMatrix(E,x, weight)
#    types <- ifelse(elm$objfn, "B", "C")
#    E <- elm$E
#    binidx <- which(elm$objfn > 0)
# 
#    if (!require(Rglpk)){
#       stop("Install 'Rglpk' to use this function")
#    }
#    sol <- Rglpk_solve_LP( obj=elm$objfn
#                  , mat=getA(E)
#                  , dir=getOps(E)
#                  , rhs=getb(E)
#                  , types=types
#                  , max=FALSE
#                  , verbose=verbose
#                  , bounds=list( lower=list(ind=seq_along(vars), val=elm$lb)
#                              , upper=list(ind=seq_along(vars), val=elm$ub)
#                              )
#                 )
#    names(sol$solution) <- c(vars,vars) 
#    list( w = sol$optimum
#        , adapt = (sol$solution > 0)[binidx]
#        , x_feasible = sol$solution[-binidx]
#        )
# }

#' Localize an error using lpSolveApi
#' localization
#' @param E editmatrix 
#' @param x named numeric with data
#' @param weight  numeric with weights
#' @return list with w, adapt and x_c
#' @keywords internal
localizeError_mip <- function( E
                            , x
                            , weight=rep(1, length(x))
                            , verbose="neutral"
                            , ...
                            ){
   vars <- getVars(E)
   elm <- buildELMatrix(E, x, weight)
   E <- elm$E
   objfn <- elm$objfn
   binidx <- which(objfn > 0)
   
   ops <- getOps(E)
   
   if (!require(lpSolveAPI)){
      stop("Install 'lpSolveAPI' to use this function")
   }
   lps <- as.lp.editmatrix(E)
   
   set.bounds(lps, lower=elm$lb, upper=elm$ub, columns=1:length(elm$lb))
   set.type(lps, columns=binidx , "binary")
   set.objfn(lps, objfn)
   
   #TODO add time.out (maxduration)
   # move univariate constraints into bounds
   lp.control(lps,presolve="rows")
 
   solve(lps)
   
   sol <- get.variables(lps)#[binidx]
   w <- get.objective(lps)
   #write.lp(lps, "test.lp")
 
   names(sol) <- c(vars,vars)
   list( w=w
       , adapt=(sol > 0)[binidx]
       , x_feasible = sol[-binidx]
       )
}

as.lp.editmatrix <- function(E){
   A <- getA(E)
   ops <- getOps(E)
   ops[ops=="=="] <- "="
   lps <- make.lp(nrow(A), ncol(A))
   dimnames(lps) <- dimnames(A)
   for (v in 1:ncol(A)){
     set.column(lps, v, A[,v])
   }
   set.constr.type(lps,types=ops)
   set.constr.value(lps, getb(E))
   lps
}

as.editmatrix.lpExtPtr <- function(x){
  #TODO
}

#testing...

# Et <- editmatrix(c(
#         "p + c == t",
#         "c - 0.6*t >= 0",
#         "c>=0",
#         "p >=0"
#         )
#                )
# 
# x <- c(p=755,c=125,t=200)
# 
# localizeError_mip(Et, x)