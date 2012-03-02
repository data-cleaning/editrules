#' Extend an editmatrix or editarray with extra constraints needed for error
#' localization with \code{mip}
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
  buildELMatrix.cateditmatrix(as.character(E), x, weight, ...)
}

#' @method buildELMatrix cateditmatrix
buildELMatrix.cateditmatrix <- function(E,x, weight=rep(1, length(x)), ...){
  #browser()
  vars <- getVars(E, type="var")
  lvls <- getVars(E, type="colnames")

  lvls <- lvls[vars %in% names(x)]
  nlvls <- length(lvls)
  
  vars <- vars[vars %in% names(x)]
  nvars <- length(vars)
  
  # pure numerical mixed edit
  if (nvars == 0){
     n <- ncol(E) - 1
     return(list( E = E
                , objfn = rep(0, n)
                , lb = rep(0, n)
                , ub = rep(1, n)
                , xlim = cbind(rep(0,n),rep(1,n))
                , binvars=sapply(getVars(E, type="colnames"), is.character)
                )
            )  
  }
  
  v <- integer(nlvls)
  names(v) <- lvls

  vx <- asCat(x)
  v[vx] <- 1
  
  #print(v)
  x <- x[vars]
  weight <- weight[match(vars, names(x))]
  
  adaptvars <- paste("adapt", vars, sep=".")
  adaptidx <- seq_along(vars) + ncol(E) - 1

  A <- getA(E)
  Aa <- matrix(0L, ncol=nvars, nrow=nrow(A))
  Ael <- cbind(A,Aa,getb(E))
  colnames(Ael)[adaptidx] <- adaptvars
  ops <- getOps(E)
  
  A <- matrix( 0L
             , ncol = ncol(Ael)-1
             , nrow = nvars
             , dimnames = list(NULL, colnames(Ael)[-ncol(Ael)])
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
             , ncol = ncol(Ael)-1
             , nrow = nvars
             , dimnames = list(NULL, colnames(Ael)[-ncol(Ael)])
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

#' Extend an editmatrix with extra constraints needed for error
#' localization
#' @method buildELMatrix editmatrix
#' @param E editmatrix 
#' @param x named numeric with data
#' @param weight vector with weights of the variable in the same order as x
#' @param xlim upper and lower boundaries of \code{x}
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix.editset <- function( E
                                 , x
                                 , weight = rep(1, length(x))
                                 , xlim = t(sapply(x, function(i) {if (is.numeric(i)) 1000*abs(i)*c(-1,1) else c(0,1)}))
                                 , maxvalue = 1e8
                                 ){
  el.E <- NULL
  
  #check xlim
  # NOTE not doing anything right now...
  xlim <- checkXlim(xlim, x)
  
  # num part
  num.vars <- getVars(E, type="num")
  num.se <- NULL
  if (!is.null(num.vars)){
    num.idx <- match(num.vars, names(x))
    num.x_i <- diag(1, nrow=length(num.vars))
    dimnames(num.x_i) <- list(num.vars,num.vars)
    num.x_0 <- as.numeric(x[num.idx])
    num.xlim <- xlim[num.idx, ,drop=FALSE]
    # create an editmatrix x_i == x^0_i
    num.E <- as.editmatrix(num.x_i, num.x_0, "==")
    num.se <- softEdits(num.E, num.xlim, prefix="adapt.")
    el.E <- c(num.se, E$num, el.E)
  }

  # cat part
  cat.vars <- getVars(E, type="cat")
  cat.se <- NULL
  if (!is.null(cat.vars)){
    cat.idx <- match(cat.vars, names(x))
    cat.A <- diag(1, nrow=length(cat.vars))
    cat.A <- cbind(cat.A,cat.A)
    cat.x_0 <- unlist(x[cat.idx])
    colnames(cat.A) <- c(asCat(cat.x_0), paste("adapt.", cat.vars, sep=""))
    cat.se <- as.editmatrix(cat.A, b=1)
    el.E <- c(cat.se, cateditmatrix(E$mixcat), el.E)
  }
  
  # mix part
  
  mix.E <- editmatrix(invert(as.character(E$mixnum)))
  mix.vars <- getVars(mix.E)
  mix.se <- NULL
  if (!is.null(mix.vars)){
    mix.idx <- which(getVars(E) %in% mix.vars)
    mix.xlim <- xlim[mix.idx,,drop=FALSE]
    mix.se <- softEdits(mix.E, xlim=mix.xlim, prefix="")
    el.E <- c(mix.se, el.E)
  }
  
#  el.E <- c(mix.se, cat.se, num.se, E$num, cateditmatrix(E$mixcat))     

  el.vars <- getVars(el.E)
  el.binvars <- sapply(el.vars, is.character)
  el.binvars[el.vars %in% num.vars] <- FALSE
  
  objfn <- sapply(el.vars, function(v) 0)
  adapt.idx <- grep("^adapt\\.", el.vars)
  adapt.nms <- names(adapt.idx) <- sub("^adapt\\.", "", el.vars[adapt.idx])
  
  objfn[adapt.idx] <- weight[match(adapt.nms, names(x))]
  
  list( E = el.E
      , objfn = objfn #sapply(vars, function(v) grepl("^adapt", v))
      , xlim = xlim
      , binvars = which(el.binvars)
      )
}

# TODO this function should check xlim and potentially adjust it, to fix (very) small boundary conditions
checkXlim <- function(xlim, x){
  xlim
}

#testing...

E <- editset(expression(
         if (x>0) y > 0
      ,  maritalstatus %in% c("married", "single")
      ,  if (maritalstatus == "married") age > 16 
      ))

x <- list(x = 1, y = -1, age=16, maritalstatus="married")
# e <- expression( pregnant %in% c(TRUE, FALSE)
#                , gender %in% c("male", "female")
#                , if (pregnant) gender == "female"
#                )
# 
# cateditmatrix(e)
# #buildELMatrix(E, x)# -> B
  localize_mip_rec(E, x=x)