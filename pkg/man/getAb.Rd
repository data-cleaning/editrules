\name{getAb}
\alias{getAb}
\title{Returns augmented matrix representation of edit set.}
\usage{
  getAb(E)
}
\arguments{
  \item{E}{\code{\link{editmatrix}}}
}
\value{
  \code{numeric} matrix \code{A|b}
}
\description{
  For a system of linear (in)equations of the form \eqn{Ax
  \odot b}, \eqn{\odot\in\{<,\leq,=\}}, the matrix
  \eqn{A|b} is called the augmented matrix.
}
\examples{

E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
print(E)
                 
# get editrules, useful for storing and maintaining the rules external from your script
as.data.frame(E)
                 
# get coeficient matrix of inequalities
getA(E)

# get augmented matrix of linear edit set
getAb(E)

# get constants of inequalities (i.e. c(0, 2))                
getb(E)

# get operators of inequalities (i.e. c("==",">"))
getOps(E)

# get variables of inequalities (i.e. c("x","y","z"))
getVars(E)

# isNormalized
isNormalized(E)

#normalized E
E <- normalize(E)
E

# is het now normalized?
isNormalized(E)
}
\seealso{
  \code{\link{editmatrix}}
  \code{\link{as.matrix.editmatrix}}
}

