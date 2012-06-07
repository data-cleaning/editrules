# test for presence of lpSolveAPI package.
checklpSolveAPI <- function(){
    nolpSolveAPI <- paste(
        "The lpSolveAPI package is required for this function.", 
        "If you have access to an internet connection it can be installed",
        "with install.packages('lpSolveAPI')",sep="\n")
    require(lpSolveAPI) || stop(nolpSolveAPI)
}

#' Localize errors using a MIP approach.
#' 
#' \code{errorLocalizer.mip} uses \code{E} and \code{x} to define a mixed integer problem
#' and solves this problem using \code{lpSolveApi}. This function can be much faster then \code{\link{errorLocalizer}} 
#' but does not return the degeneracy of a solution. However it does return an bonus: 
#' \code{x_feasible}, a feasible solution.
#'
#'
#' @param E an \code{\link{editset}}, \code{\link{editmatrix}}, or \code{\link{editarray}}
#' @param x named \code{numeric} with data
#' @param weight  \code{numeric} with weights
#' @param maxduration number of seconds that is spent on finding a solution
#' @param verbose verbosity argument that will be passed on to \code{solve} lpSolveAPI
#' @param ... other arguments that will be passed on to \code{solve} or to generateXlims.
#' @return list with solution weight \code{w}, \code{logical} \code{adapt} stating what to adapt,  
#'  \code{x_feasible} and the lp problem (an \code{lpExtPtr} object)
#'
#' @seealso \code{\link{localizeErrors}}, \code{\link{errorLocalizer}}, \code{\link{errorLocation}}
#'
#' @references
#'  E. De Jonge and Van der Loo, M. (2012) Error localization as a mixed-integer program in 
#'  editrules (included with the package)
#'
#'  lp_solve and Kjell Konis. (2011). lpSolveAPI: R Interface for
#'  lp_solve version 5.5.2.0. R package version 5.5.2.0-5.
#'  http://CRAN.R-project.org/package=lpSolveAPI
#'
errorLocalizer.mip <- function( E
                            , x
                            , weight=rep(1, length(x))
                            , maxduration=600
                            , verbose="neutral"
                            , ...
                            ){

   vars <- getVars(E)
   E <- as.editset(E)

    # perturb weights for randomized selection from equivalent solutions
   wp <- perturbWeights(as.vector(weight))
   
   t.start <- proc.time()
   elm <- buildELMatrix(E=E, x=x, weight=wp, ...)
   
   Ee <- elm$E
   objfn <- elm$objfn
   ops <- getOps(Ee)
   lps <- as.lp.editmatrix(Ee, obj=elm$objfn, xlim=elm$xlim)
   # TODO move this code into as.lp.editmatrix
    ## the following code...
    #   set.bounds(lps, lower=elm$xlim[,1], upper=elm$xlim[,2], columns=1:nrow(elm$xlim))
    ## attempted to set bounds at non-existent columns 
    ## this is better solved in buildELMatrix (mvdl)
   icol <- match(rownames(elm$xlim),colnames(lps),nomatch=0)
   lo <- elm$xlim[icol>0,1]
   up <- elm$xlim[icol>0,2]
   set.bounds(lps, lower=lo, upper=up, columns=icol[icol>0]) 
   set.type(lps, columns=elm$binvars , "binary")
   set.objfn(lps, objfn)
   # end TODO
   lp.control( lps
             , presolve = "rows"    # move univariate constraints into bounds
             , timeout = maxduration
             , epsint = 1e-8
             )
   #print(lps)

    
   statuscode <- solve(lps)
   degeneracy <- get.solutioncount(lps)
   
   sol <- get.variables(lps)
   # lps may have optimized and removed redundant adapt.variables, so retrieve names of variable...
   names(sol) <- colnames(lps)
   ## weights are perturbed so objective function is recalculated as sum(weights[adapt])
   # w <- get.objective(lps)
   # get the positions of the adapt.variables
   aidx <- grepl("^adapt\\.", names(sol))
   # split solution in a value and a adapt part
   sol.adapt <- sol[aidx]
   sol.values <- sol[!aidx]
   
   cat.idx <- names(sol.values) %in% names(elm$binvars)
   sol.cat <- asLevels(sol.values[cat.idx])
   sol.num <- sol.values[!cat.idx]
   
   #print(list(sol.cat=sol.cat, sol.num=sol.num))
   
   names(sol.adapt) <- sub("^adapt\\.","",names(sol.adapt))
   
   #write.lp(lps, "test.lp")
   #print(list(idx=idx, sol=sol))
   adapt <- sapply(x, function(i) FALSE)
   adapt[names(sol.adapt)] <- (sol.adapt > 0)

   x_feasible <- x
   idx <- match(names(sol.values), names(x), nomatch=0)
   
   x_feasible[names(sol.num)] <- sol.num
   
   if (length(sol.cat))
      x_feasible[names(sol.cat)] <- sol.cat
#    if (is.cateditmatrix(E)){
#      x_feasible[idx] <- asLevels(sol.values)
#    } else {
#      x_feasible[idx] <- sol.values
#    }
   

   t.stop <- proc.time()
   duration <- t.stop - t.start
   list( w=sum(weight[adapt])
       , adapt = adapt
       , x_feasible = x_feasible
       , duration = duration
       , maxdurationExceeded = unname(duration[3] >= maxduration)
       , statuscode = statuscode
       , degeneracy = degeneracy
       , lp=lps
       )
}

# assumes that E is normalized!
as.lp.editmatrix <- function(E, obj, xlim, type){
   require(lpSolveAPI)
   epsb <- 1e-7
   A <- getA(E)
   lps <- make.lp(nrow(A), ncol(A))
   dimnames(lps) <- dimnames(A)   
   for (v in 1:ncol(A)){
     set.column(lps, v, A[,v])
   }
   ops <- getOps(E)
   ops[ops=="=="] <- "="
   lt <- ops == "<"
   ops[lt] == "<="
   set.constr.type(lps,types=ops)

   b <- getb(E)
   Ab <- abs(getAb(E))
   
   maxA <- max(Ab)
   rangeA <- range(Ab[Ab>0])
   m <- rangeA[1]/rangeA[2]
   
   #print(epsb/m)
   #print(range(Ab[Ab>0]))
   # adjust boundaries for less than 
   
   b[lt] <- (b[lt] - m)
   set.constr.value(lps, b)
   #print(list(maxA=maxA, lps=lps))
   lps
}

asCat <- function(x, sep=":", useLogicals=TRUE){
  nms <- paste(names(x),x, sep=sep)
  if (useLogicals) {
    idx <- x %in% c("TRUE", "FALSE")
    nms[idx] <- names(x)[idx]
  }
  names(nms) <- names(x)
  nms
}

#' Transform a found solution into a categorical record
#' @keywords internal
asLevels <- function(x){
  vars <- sub(":.+", "", names(x))
  lvls <- sub(".+:", "", names(x))
  logicals <- vars == lvls
  lvls[logicals] <- as.character(x[logicals] > 0)
  names(lvls) <- vars
  lvls[x > 0 | logicals]
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
# errorLocalizer.mip(Et, x)
# 
# Et2 <- editmatrix(expression(
#   p + c == t    
#   ))
# x <- c(p=75,c=125,t=300)
# errorLocalizer.mip(Et2, x)  # random?
# errorLocalizer.mip(Et2, x, weight=c(1,1,1))  # random?
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
#   errorLocalizer.mip(Et, x)
#   errorLocalizer.mip(Ec, r)
# # # asCat(r)
# #  
