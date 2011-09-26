# test for presence of lpSolveAPI package.
checklpSolveAPI <- function(){
    nolpSolveAPI <- paste(
        "The lpSolveAPI package is required for this function.", 
        "If you have access to an internet connection it can be installed",
        "with install.packages('lpSolveAPI')",sep="\n")
    require(lpSolveAPI) || stop(nolpSolveAPI)
}

 
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
  
  #TODO cope with NA's in x (choose upper en lower bound and  don't generate error localization constraints for na's')
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
  r_lower <- diag(lb-x)
  r <- cbind(r_x, r_lower, -x)
  Ael <- rbind(Ael, r)
   
  r_x <- diag(1, nvars)
  r_upper <- diag(x-ub)
  r <- cbind(r_x, r_upper, x)
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

#' Localize an error using lpSolveApi
#' localization
#' @param E editmatrix 
#' @param x named numeric with data
#' @param weight  numeric with weights
#' @param duration number of seconds that is spent on finding a solution
#' @return list with w, adapt and x_c
#' @keywords internal
localizeErrors_mip_rec <- function( E
                                  , x
                                  , weight=rep(1, length(x))
                                  , maxduration=600
                                  , verbose="neutral"
                                  , ...
                                  ){
   checklpSolveAPI()
   
   if (is.editarray(E)){
     E <- cateditmatrix(as.character(E, datamodel=FALSE))
   }   
   
   t <- proc.time()
   vars <- getVars(E)
   # vars <- sample(vars)
   # E <- E[, vars]
   idx <- match(vars, names(x))
   elm <- buildELMatrix(E, x, weight)
   E <- elm$E
   objfn <- elm$objfn
   binidx <- which(objfn > 0)
   
   ops <- getOps(E)
   lps <- as.lp.editmatrix(E)
   
   set.bounds(lps, lower=elm$lb, upper=elm$ub, columns=1:length(elm$lb))
   set.type(lps, columns=binidx , "binary")
   set.objfn(lps, objfn)
   
   #TODO add time.out (maxduration)
   # move univariate constraints into bounds
   lp.control(lps,presolve="rows", timeout=maxduration)
 
   solve(lps)
   
   sol <- get.variables(lps)#[binidx]
   w <- get.objective(lps)
   #write.lp(lps, "test.lp")
   
   names(sol) <- c(vars,vars)
   
   adapt <- sapply(x, function(i) FALSE)
   adapt[idx] <- (sol > 0)[binidx]
   
   x_feasible <- x
   x_feasible[idx] <- sol[-binidx]
   
   duration <- getDuration(t - proc.time())
   list( w=w
       , adapt=adapt
       , x_feasible = x_feasible
       , duration = duration
       , maxdurationExceeded = unname(duration[3] >= maxduration)
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
   
#' Localize errors on records in a data.frame.
#' 
#' Loops over all records in \code{dat} and performs error localization with \code{\link{localizeError_mip_rec}}.
#' For each record it finds the smallest (weighted) number of variables to be imputed or adapted
#' such that all violated edits can be satisfied, without violating new ones. If there are multiple
#' optimal (equally weighted) solutions a random solution is chosen. 
#'
#'
#' @param E an object of class \code{\link{editmatrix}} or \code{\link{editarray}}
#' @param dat a \code{data.frame} with variables in E.
#' @param ... options to be passed to \code{\link{errorLocalizer}}
#'
#' @return an object of class \code{errorLocation}
#' @export
localizeErrors_mip <- function(E, dat, ...){
    stopifnot(is.data.frame(dat))
    user <- Sys.info()['user']
    call <- sys.call()

    n <- nrow(dat)
    m <- ncol(dat)
    err <- array( NA
                , dim=c(n,m)
                , dimnames = list( record = rownames(dat)
                                 ,  adapt = colnames(dat)
                                 )
                )   
    duration <- array( 0
                     , dim = c(n,3)
                     , dimnames = list( record = rownames(dat)
                                      , duration = c('user','system','elapsed')
                                      )
                     )
    weight <- rep(NA,n)
    degeneracy <- rep(NA,n)
    maxDurationExceeded <- logical(n)
    X <- t(dat)
    for ( i in 1:n ){
        r <- X[,i]
        e <- localizeErrors_mip_rec(E, r, ...)
        duration[i,] <- getDuration(e$duration)
        maxDurationExceeded[i] <- e$maxdurationExceeded
    }
    structure(
        list(
            adapt  = err,
            status = data.frame(
                weight     = weight, 
                degeneracy = degeneracy, 
                duration,
                maxDurationExceeded
            ),
            call = call,
            user = user,
            timestamp = date()
        ),
        class=c('errorLocation','list')
    ) 
}


#testing...

Et <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0",
        "c>=0",
        "p >=0"
        )
               )

x <- c(p=755,c=125,t=200)

localizeErrors_mip_rec(Et, x)