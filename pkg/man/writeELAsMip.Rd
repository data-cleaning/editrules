\name{writeELAsMip}
\alias{writeELAsMip}
\title{Rewrite an editset and reported values into the components needed for a mip solver}
\usage{
  writeELAsMip(E, x, weight = rep(1, length(x)), M = 1e+07,
    epsilon = 0.001, ...)
}
\arguments{
  \item{E}{\code{\link{editset}} or any object that is
  coercable to an editset.}

  \item{x}{named \code{list} or \code{vector} with data}

  \item{weight}{vector with weights of the variable in the
  same order as x}

  \item{M}{maximum allowed difference between reported
  value and corrected value}

  \item{epsilon}{offset needed for writing a strict
  inequality into a an inequality}
}
\value{
  list with an editmatrix, objfn, binvars, numvars, M and
  epsilon
}
\description{
  Rewrite an editset and reported values into the
  components needed for a mip solver
}
\keyword{internal}

