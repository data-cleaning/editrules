\name{as.mip}
\alias{as.mip}
\title{Write an editset into a mip representation}
\usage{
as.mip(E, x = NULL, weight = NULL, M = 1e+07, epsilon = 0.001,
  prefix = "delta.", ...)
}
\arguments{
  \item{E}{an \code{link{editset}} or an object that is
  coerciable to an \code{editset}}

  \item{x}{named \code{list}/\code{vector} with variable
  values}

  \item{weight}{reliability weights for values of \code{x}}

  \item{M}{Constant that is used for allowing the values to
  differ from \code{x}}

  \item{epsilon}{Constant that is used for converting '<'
  into '<='}

  \item{prefix}{prefix for dummy variables that are
  created}

  \item{...}{not used}
}
\value{
a mip object containing al information for transforming it
into an lp/mip problem
}
\description{
Writes an editset or an object coercable to an editset as a
mip problem.
}

