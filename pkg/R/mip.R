#' Write an editset into a mip representation 
#' 
#' Write an editset as a mip problem. 
#' Note that the objective function is empty
#' @param E an \code{link{editset}} or an object that is coerciable to an 
#' {\code editset}
as.mip <- function(E, objfn = NULL, M=1e7, epsilon=1e-3,...){
  E <- as.editset(E)
  
  E_mip = c( E$num
           , cateditmatrix(E$mixcat)
           , softEdits(editmatrix(invert(as.character(E$mixnum))), prefix="") 
           )
  
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

# # quick test 
# E <- editset(c(r1="x > 1","y >= x", r2="if (x>1) y> 2", r3="A %in% c('a', 'b')"))
# as.mip(E)
