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
#' @param xlim optional \code{list} with upper and lower boundaries of \code{x}
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix.editmatrix <- function( E
                                    , x
                                    , weight = rep(1, length(x))
                                    , xlim = t(sapply(x, function(i) {if (is.numeric(i)) 1000*abs(i)*c(-1,1) else c(0,1)}))
                                    , maxvalue = 1e8
                                    ){
  #TODO sample order of variables
  E <- E[, c(sample(length(getVars(E))), ncol(E))]
  x <- unlist(x)
  
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
#' @keywords internal
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
#' @method buildELMatrix editset
#' @param E editmatrix 
#' @param x named numeric with data
#' @param weight vector with weights of the variable in the same order as x
#' @param xlim upper and lower boundaries of \code{x}
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix.editset <- function( E
                                 , x
                                 , weight = rep(1, length(x))
                                 , xlim = generateXlims(x)
                                 , maxvalue = 1e8
#                                 , editweights = rep(Inf, nrow(E))
                                 ){
  #check xlim
  # NOTE not doing anything right now...
  xlim <- checkXlim(xlim, x)
  
  el.E <- NULL
    
#   soft <- is.finite(editweights)
#   if (any(soft)){
#     soft.E <- E[soft,]
#     soft.weights <- editweights[soft]
#     
#     #TODO process softedits into el.E
#     soft.num <- softEdits(soft.E$num, xlim, prefix=".softnum.")
#     soft.cat <- softEdits(cateditmatrix(soft.E$mixcat), xlim,prefix=".softcat."")
#     el.E <- c(soft.num, soft.cat, el.E)
#     E <- E[!soft]
#   }
    
  # num part
  num.vars <- getVars(E, type="num")
  #TODO check NA values...
  if (!is.null(num.vars)){
    num.idx <- match(num.vars, names(x))
    num.x <- diag(1, nrow=length(num.vars))
    dimnames(num.x) <- list(num.vars,num.vars)
    num.x0 <- unlist(x[num.idx])
    num.xlim <- xlim[num.idx,,drop=FALSE]
    # create an editmatrix x_i == x^0_i
    num.E <- as.editmatrix(num.x, num.x0, "==")
    num.se <- softEdits(num.E, num.xlim, prefix="adapt.")
    el.E <- c(num.se, E$num, el.E)
  }

  # cat part
  cat.vars <- getVars(E, type="cat")
  #TODO check NA values...
  if (!is.null(cat.vars)){
    cat.idx <- match(cat.vars, names(x))
    cat.A <- diag(1, nrow=length(cat.idx))
    cat.A <- cbind(cat.A,cat.A)
    cat.x_0 <- unlist(x[cat.idx])
    
    colnames(cat.A) <- c(asCat(cat.x_0), paste("adapt.", cat.vars, sep=""))
    # check for non existing levels (including NA's)
    cat.b <- ifelse(asCat(cat.x_0, useLogicals=FALSE) %in% getlevels(E$mixcat), 1, 2)
    cat.se <- as.editmatrix(cat.A, b=cat.b)
    el.E <- c(cat.se, cateditmatrix(E$mixcat), el.E)
  }
  
  # mix part  
  mix.E <- editmatrix(invert(as.character(E$mixnum)))
  mix.vars <- getVars(mix.E)
  if (!is.null(mix.vars)){
    mix.idx <- match(mix.vars, names(x))
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

#' Utility function for generating sensible boundaries for variables
#' Needed for mip error localization.
#' 
#' This function determines the minimum and maximum value in \code{x} and 
#' applies an offset to it. NA values will be treated as zero.
#'
#' @param x \code{data vector}
#' @param factor multiplicative factor for range of x
#' @param offset offset added to range of x
#' @param maxvalue Not used
#' @param ... not used
#' @return a lower and upper boundary of \code{x}
createXlim <- function(x, factor=1, offset=c(-1000,1000), maxvalue=1e8, ...){
  if (!is.numeric(x)){
    return(c(0,1))
  }
  
  x[is.na(x)] <- 0
  xlim <- factor*c(min(x), max(x)) + offset
  return(xlim)
}

generateXlims <- function(x, xlim=list(), create=createXlim, ...){
  boundaries <- lapply(x, createXlim, ...)
  for (var in names(xlim)){
    boundaries[[var]] <- xlim[[var]]    
  }
  t(sapply(boundaries, c))
}

checkXlim <- function(xlim, x, maxvalue=1e8){
  # expand list
  if (is.list(xlim)){
    xlim2 <- t(sapply(x, function(i) {if (is.numeric(i)) 1000*abs(i)*c(-1,1) else c(0,1)}))
    for (var in names(xlim)) { 
      xlim2[var,] <- xlim[[var]]
    }
    xlim <- xlim2
  }
  
  xlim[is.na(xlim[,1]),] <- -maxvalue
  xlim[is.na(xlim[,2]),] <- maxvalue
  xlim
}

#testing...

# E <- editset(expression(
#          if (x>0) y > 0
#       ,  maritalstatus %in% c("married", "single")
#       ,  if (maritalstatus == "married") age >= 17
#       ))
# 
# x <- list(x = 1, y = -1, age=16, maritalstatus="married")
# #x <- list(x = 1, y = -1, age=16, maritalstatus=NA)
# # e <- expression( pregnant %in% c(TRUE, FALSE)
# #                , gender %in% c("male", "female")
# #                , if (pregnant) gender == "female"
# #                )
# # 
# # cateditmatrix(e)
# checkXlim(list(age=c(0,200)), x)
# 
# buildELMatrix(E, x)# -> B
# localize_mip_rec(E, x=x)
