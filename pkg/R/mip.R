#' Write an editset into a mip representation 
#' 
#' Write an editset as a mip problem. 
#' Note that the objective function is empty
#' @param E an \code{link{editset}} or an object that is coerciable to an 
#' \code{editset}
#' @param M Constant that is used for allowing softconstraints
#' @param epsilon Constant that is used for converting '<' into '<='
#' @param ... not used 
#' @return a mip object containing al information for transforming it 
#' into an lp/mip problem
#' @export
as.mip <- function(E, x=NULL, weight=NULL, M=1e7, epsilon=1e-3,...){
  E <- as.editset(E)
  objfn <- NULL
  
  E_mip = c( E$num
           , cateditmatrix(E$mixcat)
           , softEdits(editmatrix(invert(as.character(E$mixnum))), prefix="") 
           )
  
  if (!missing(x)){
    if (missing(weight)){
      weight <- rep(1.0, length(x))
    } else {
      stopifnot(length(weight) == length(x))
    }
    
    names(weight) <- paste0("delta.", names(x))
    
    num_vars <- getVars(E, type="num")
    num_idx <- match(num_vars, names(x))
    num <- na.omit(x[num_idx])
    A0_num <- editmatrix(paste(names(num),"==", num))
    rownames(A0_num) <- names(num)
    
    cat_vars <- getVars(E, type="cat")
    cat_idx <- match(cat_vars, names(x))
    cat <- na.omit(x[cat_idx])
    A0_cat <- cateditmatrix(paste(names(cat),"==", cat))
    rownames(A0_cat) <- names(cat)
    
    E_mip <- c(E_mip, softEdits(A0_num), softEdits(A0_cat))
    
    objfn <- sapply(colnames(E_mip), function(v) { weight[v] })
    objfn[is.na(objfn)] <- 0
  }
  
  # replace strict inequalities...
  A <- getA(E_mip)
  b <- getb(E_mip)
  ops <- getOps(E_mip)
  lt <- ops == "<"
  b[lt] <- b[lt] - epsilon
  ops[lt] <- "<="
  E_mip = as.editmatrix(A=A, b=b, ops=ops)
  
  binvars = !sapply(getVars(E_mip), `%in%`, getVars(E, type="num"))
  
  structure(
    list( E = E_mip
        , objfn = objfn
        , binvars = which(binvars)
        , M = M
        , epsilon = epsilon
    ),
    class="mip"
  )
}

#' @S3method print mip
print.mip <- function(x, ...){
  print.editmatrix(x$E, textOnly=T)
  if (!is.null(x$objfn)) {
    idx <- x$objfn != 0
    of <- paste0(x$objfn, "*", colnames(x$E))[idx]
    of <- paste(of, collapse=" + ")    
    cat("objective function = min: ", of, "\n")
  }
}

# # quick test 
# E <- editset(c(r1="x > 1","y >= x", r2="if (x>1) y> 2", r3="A %in% c('a', 'b')"))
# as.mip(E)
