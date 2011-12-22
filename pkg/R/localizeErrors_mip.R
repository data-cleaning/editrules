# test for presence of lpSolveAPI package.
checklpSolveAPI <- function(){
    nolpSolveAPI <- paste(
        "The lpSolveAPI package is required for this function.", 
        "If you have access to an internet connection it can be installed",
        "with install.packages('lpSolveAPI')",sep="\n")
    require(lpSolveAPI) || stop(nolpSolveAPI)
}

#' Extend an editmatrix or editarray with extra constraints needed for error
#' localization
#' @param E editmatrix 
#' @param x named numeric with data
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix <- function(E,x,weight,...){
  UseMethod("buildELMatrix")
}

#' Extend an editmatrix with extra constraints needed for error
#' localization
#' @method buildELMatrix editmatrix
#' @param E editmatrix 
#' @param x named numeric with data
#' @param weight vector with weights of the variable in the same order as x
#' @param xlim upper and lower boundaries of \code{x}
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix.editmatrix <- function( E
                                    , x
                                    , weight = rep(1, length(x))
                                    , xlim = 1000 * cbind(l=-abs(x), u=abs(x))
                                    , maxvalue = 1e8
                                    ){
  #TODO sample order of variables
  E <- E[, c(sample(length(getVars(E))), ncol(E))]
  vars <- getVars(E)
  idx <- match(vars, names(x))
  
  weight <- weight[idx]
  xlim <- xlim[idx,,drop=FALSE]
  x <- x[idx]
  nvars <- length(vars)
  
  adaptvars <- paste("adapt", vars, sep=".")
  adaptidx <- seq_along(vars) + nvars
  
  ub <- xlim[,2]
  lb <- xlim[,1]
  ub[is.na(ub)] <- maxvalue
  ub[round(x) == 0] <- maxvalue # to cope with very small (or 0) values
  lb <- -ub
  x[is.na(x)] <- 2 * (ub[is.na(x)] + 1) # put value for NA's out of bound
  
  A <- getA(E)
  
  Ael <- cbind(A,A,getb(E))
  colnames(Ael)[adaptidx] <- adaptvars
  Ael[,adaptidx] <- 0
       
  r_x <- diag(-1, nvars)
  if ( nvars == 1 ){
    r_lower = matrix(lb-x,ncol=1,nrow=1)
  } else {
    r_lower <- diag(lb-x)
  }
  r <- cbind(r_x, r_lower, -x)
  Ael <- rbind(Ael, r)
   
  r_x <- diag(1, nvars)
  if ( nvars == 1 ){
    r_upper <- matrix(x-ub,ncol=1,nrow=1)
  } else {
    r_upper <- diag(x-ub)
  }
  r <- cbind(r_x, r_upper, x)
  Ael <- rbind(Ael, r)
    
  ops <- c(getOps(E), rep("<=", 2*nvars))
  
  nb <- ncol(Ael)
  
  # remove NA rows
  Ena <- !is.na(Ael[,nb])
  Ael <- Ael[Ena,,drop=FALSE]
  ops <- ops[Ena]
    
  Eel <- as.editmatrix( Ael[,-nb,drop=FALSE]
                      , Ael[,nb]
                      , ops
                      )
  
  objfn <- numeric(2*nvars)
  objfn[adaptidx] <- weight
  
  names(lb) <- names(ub) <- vars
  names(objfn) <- getVars(Eel)
  binvars <- which(objfn > 0)
  xlim <- cbind(lower=lb, upper=ub)
  
  list( E = Eel
      , objfn = objfn
      , lb=lb
      , ub=ub
      , binvars=binvars
      , xlim=xlim
      )
}

#' @method buildELMatrix editarray
buildELMatrix.editarray <- function(E,x, weight=rep(1, length(x)), ...){
  buildELMatrix.cateditmatrix(cateditmatrix(E), x, weight, ...)
}

#' @method buildELMatrix cateditmatrix
buildELMatrix.cateditmatrix <- function(E,x, weight=rep(1, length(x)), ...){
  vars <- getVars(E)
  nvars <- length(vars)
  lvls <- colnames(E)[-ncol(E)]
  nlvls <- length(lvls)
  
  v <- integer(nlvls)
  names(v) <- lvls
  
  vx <- asCat(x)
  v[vx] <- 1
  
  #print(v)
  x <- x[vars]
  weight <- weight[match(vars, names(x))]
  
  adaptvars <- paste("adapt", vars, sep=".")
  adaptidx <- seq_along(vars) + nlvls
    
  A <- getA(E)
  Aa <- matrix(0L, ncol=nvars, nrow=nrow(A))
  Ael <- cbind(A,Aa,getb(E))
  colnames(Ael)[adaptidx] <- adaptvars
  ops <- getOps(E)
  
  A <- matrix( 0L
             , ncol = nvars+nlvls
             , nrow = nvars
             , dimnames = list(NULL, c(lvls, adaptvars))
             )
  
  for (i in seq_along(vx)){
    A[i,vx[i]] <- 1
    A[i,paste("adapt",names(vx[i]), sep=".")] <- 1
  }
  A <- cbind(A, 1)

  Ael <- rbind(Ael, A)
  ops <- c(ops, rep("==", nrow(A)))
  
  # domain constraints, (in case of open domains)
  A <- matrix( 0L
             , ncol = nvars+nlvls
             , nrow = nvars
             , dimnames = list(NULL, c(lvls, adaptvars))
             )
  
  nlvls <- sub(":.+","", lvls)
  #print(nlvls)
  for (i in seq_along(vars)){
    A[i,which(nlvls==vars[i])] <- 1
  }
  A <- cbind(A, 1)
  
  Ael <- rbind(Ael, A)
  ops <- c(ops, rep("<=", nrow(A)))
  
  nb <- ncol(Ael)
  
  Eel <- as.editmatrix( Ael[,-nb,drop=FALSE]
                      , Ael[,nb]
                      , ops
                      )
  
  objfn <- lb <- ub <- Ael[1,-nb]
  lb[] <- 0
  ub[] <- 1
  objfn[] <- 0
  xlim <- cbind(lower=objfn, upper=objfn+1)
  objfn[adaptidx] <- weight
  
  binvars <- seq_along(objfn)
    
  list( E = Eel
      , objfn = objfn
      , lb = lb
      , ub = ub
      , xlim = xlim
      , binvars=binvars
      )
}

#' Localize errors by using lpSolveApi
#' 
#' \code{localize_mip_rec} uses \code{E} and \code{x} to define a mixed integer problem
#' and solves this problem using \code{lpSolveApi}. 
#' This function can be much faster then \code{errorLocalizer} but does not return the degeneracy
#' of a solution. However it does return an bonus: \code{x_feasible}, a feasible solution
#' @param E editmatrix 
#' @param x named numeric with data
#' @param weight  numeric with weights
#' @param maxduration number of seconds that is spent on finding a solution
#' @return list with w, adapt and x_c
#' @keywords internal
localize_mip_rec <- function( E
                            , x
                            , weight=rep(1, length(x))
                            , maxduration=600
                            , verbose="neutral"
                            , ...
                            ){
   checklpSolveAPI()
   
   if (is.editarray(E)){
     E <- cateditmatrix(as.character(E))
   }   
   
   t.start <- proc.time()
   elm <- buildELMatrix(E, x, weight, ...)
   #print(elm)
   Ee <- elm$E
   objfn <- elm$objfn
   adaptidx <- which(objfn > 0)
   
   ops <- getOps(Ee)
   lps <- as.lp.editmatrix(Ee)
   
   set.bounds(lps, lower=elm$xlim[,1], upper=elm$xlim[,2], columns=1:length(elm$lb))
   set.type(lps, columns=elm$binvars , "binary")
   set.objfn(lps, objfn)
   
   lp.control( lps
             , presolve = "rows"    # move univariate constraints into bounds
             , timeout = maxduration
             , epsint = 1e-8
             )
 
   statuscode <- solve(lps)
   degeneracy <- get.solutioncount(lps)
   
   sol <- get.variables(lps)#[adaptidx]
   w <- get.objective(lps)
   #write.lp(lps, "test.lp")
   
   vars <- getVars(Ee)
   idx <- match(vars[-adaptidx], names(x), nomatch=0)
   names(sol) <- vars
   #print(list(idx=idx, sol=sol))
   
   adapt <- sapply(x, function(i) FALSE)
   adapt[idx] <- (sol[adaptidx] > 0)
   
   x_feasible <- x
   if (is.cateditmatrix(E)){
     x_feasible[idx] <- asLevels(sol[-adaptidx])
   } else {
     x_feasible[idx] <- sol[-adaptidx]
   }
   t.stop <- proc.time()
   duration <- t.stop - t.start
   list( w=w
       , adapt = adapt
       , x_feasible = x_feasible
       , duration = duration
       , maxdurationExceeded = unname(duration[3] >= maxduration)
       , statuscode = statuscode
       , degeneracy = degeneracy
       )
}

as.lp.editmatrix <- function(E){
   require(lpSolveAPI)
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

asCat <- function(x){
  nms <- paste(names(x),x, sep=":")
  is.na(nms) <- is.na(x)
  names(nms) <- names(x)
  nms
}

#' Transform a found solution into a categorical record
#' @keywords internal
asLevels <- function(x){
  vars <- sub(":.+", "", names(x))
  lvls <- sub(".+:", "", names(x))
  names(lvls) <- vars
  lvls[x > 0]
}
   
#testing...

# Et <- editmatrix(expression(
#         p + c == t,
#         c - 0.6*t >= 0,
#         c>=0,
#         p >=0
#         )
#                )
# 
# x <- c(p=755,c=125,t=200)
# 
# localize_mip_rec(Et, x)
# 
# Et2 <- editmatrix(expression(
#   p + c == t    
#   ))
# x <- c(p=75,c=125,t=300)
# localize_mip_rec(Et2, x)  # random?
# localize_mip_rec(Et2, x, weight=c(1,1,1))  # random?
# 
# 
# 
# Es <- c(
#   "age %in% c('under aged','adult')",
#   "maritalStatus %in% c('unmarried','married','widowed','divorced')",
#   "positionInHousehold %in% c('marriage partner', 'child', 'other')",
#   "if( age == 'under aged' ) maritalStatus == 'unmarried'",
#   "if( maritalStatus %in% c('married','widowed','divorced')) !positionInHousehold %in% c('marriage partner','child')"
#   )
# Ec <- cateditmatrix(c(
#   "age %in% c('under aged','adult')",
#   "maritalStatus %in% c('unmarried','married','widowed','divorced')",
#   "positionInHousehold %in% c('marriage partner', 'child', 'other')",
#   "if( age == 'under aged' ) maritalStatus == 'unmarried'",
#   "if( maritalStatus %in% c('married','widowed','divorced')) !positionInHousehold %in% c('marriage partner','child')"
#   ))
# Ec
# r <- c(age = 'under aged', maritalStatus='married', positionInHousehold='child')
# # buildELMatrix(Et,x)
# # buildELMatrix(Ec,r)
#   localize_mip_rec(Et, x)
#   localize_mip_rec(Ec, r)
# # # asCat(r)
# #  
