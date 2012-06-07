#' Extend an editset with extra constraints needed for error
#' localization
#' @param E editset
#' @param x named list with data
#' @param weight vector with weights of the variable in the same order as x
#' @param xlim upper and lower boundaries of \code{x}
#' @return list with extended E, objfn and lower and upper bound
#' @keywords internal
buildELMatrix <- function( E
                         , x
                         , weight = rep(1, length(x))
                         , xlim = generateXlims(x)
                         , maxvalue = 1e8
#                          , editweight = rep(Inf, nedits(E))
#                          , lambda = if(any(is.finite(editweight))) 0.5 else 1
                         , ...
                         ){
  xlim <- checkXlim(xlim, x)
  
  el.E <- NULL
    
#   soft <- is.finite(editweight)
#   if (any(soft)){
#     soft.E <- E[soft,]
#     soft.weights <- editweight[soft]
#     
#     #TODO process softedits into el.E
#     soft.num <- softEdits(soft.E$num, xlim, prefix=".soft.")
#     
#     #TODO column with diag(1, nrow(soft.E$mixcat))
#     soft.cat <- NULL
#     #soft.cat <- softEdits(cateditmatrix(soft.E$mixcat),xlim,prefix=".softcat.")
#     el.E <- c(soft.num, soft.cat, el.E)
#     E <- E[!soft,]
#   }
    
  # num part
  num.vars <- getVars(E, type="num")
  
  if (!is.null(num.vars)){
    num.idx <- match(num.vars, names(x))
    num.x <- diag(1, nrow=length(num.vars))
    dimnames(num.x) <- list(num.vars,num.vars)
    num.x0 <- unlist(x[num.idx])
    num.xlim <- xlim[num.idx,,drop=FALSE]
    # create an editmatrix x_i == x^0_i
    num.E <- as.editmatrix(num.x, num.x0)
    num.se <- softEdits(num.E, num.xlim, prefix="adapt.")
    el.E <- c(num.se, E$num, el.E)
  }

  # cat part
  cat.vars <- getVars(E, type="cat")
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

#   if (any(soft)){
#      soft.idx <- grep("^\\.soft", el.vars)
#      objfn[soft.idx] <- (1-lambda) * soft.weights
#   }
  
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
#' applies an offset to it. In case of NA values will be treated as zero.
#'
#' @param x \code{data vector}
#' @param factor multiplicative factor for range of x
#' @param offset offset added to range of x
#' @param na.rm \code{logical} If set to \code{TRUE} NA's will be treated as zero's, otherwise if x contains NA's minvalue and maxvalue will be returned 
#' @param minvalue If x contains \code{NA} and na.rm is \code{FALSE}, the returned xlim will have minvalue as lower boundary
#' @param maxvalue If x contains \code{NA} and na.rm is \code{FALSE}, the returned xlim will have maxvalue as upper boundary
#' @param ... not used
#' @return a lower and upper boundary of \code{x}
#' @keywords internal
createXlim <- function(x, factor=1, offset=c(-1000,1000), na.rm = FALSE, maxvalue=1e8, minvalue=-maxvalue, ...){
  if (!is.numeric(x)){
    return(c(0,1))
  }
  
  if (na.rm){
    x[is.na(x)] <- 0
  }
  
  if (any(is.na(x))){
    return(c(minvalue, maxvalue))
  }
 m <- max(abs(x)+1)
 c(max(-m*100,minvalue ),min(m*100,maxvalue)) 
#  factor*c(min(x), max(x)) + offset
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
    xlims <- generateXlims(x, xlim, maxvalue=maxvalue)
    #xlim2 <- t(sapply(x, function(i) {if (is.numeric(i)) 1000*abs(i)*c(-1,1) else c(0,1)}))
    for (var in names(xlim)) { 
      xlims[var,] <- xlim[[var]]
    }
    xlim <- xlims
  }
  
  #xlim[is.na(xlim[,1]),] <- -maxvalue
  #xlim[is.na(xlim[,2]),] <- maxvalue
  xlim
}

#testing...

# E <- editset(expression(
#          if (x>0) y > 0
#       ,  maritalstatus %in% c("married", "single")
#       ,  if (maritalstatus == "married") age >= 17
#       ))
# # 
# x <- list(x = 1, y = -1, age=16, maritalstatus="married")
# # #x <- list(x = 1, y = -1, age=16, maritalstatus=NA)
# # # e <- expression( pregnant %in% c(TRUE, FALSE)
# # #                , gender %in% c("male", "female")
# # #                , if (pregnant) gender == "female"
# # #                )
# # # 
# # # cateditmatrix(e)
# # checkXlim(list(age=c(0,200)), x)
# # 
# buildELMatrix(E, x, editweight=c(Inf, 1), xlim=list(age=c(0,200)))# -> B
# errorLocalizer.mip(E, x=x,, xlim=list(age=c(0,200)))
