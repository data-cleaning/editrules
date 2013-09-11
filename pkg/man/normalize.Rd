\name{normalize}
\alias{normalize}
\title{Normalizes an editmatrix}
\usage{
  normalize(E)
}
\arguments{
  \item{E}{\code{\link{editmatrix}}}
}
\value{
  If E was normalized, the original editmatrix is returned,
  otherwise a new normalized editmatrix will be returned
}
\description{
  An set of linear edits of the form \eqn{{\bf a}\cdot{\bf
  x}\odot b} with is called normalized when all
  \eqn{\odot\in\{==,\leq,<\}}
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
}

