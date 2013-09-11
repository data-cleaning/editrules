\name{as.mip}
\alias{as.mip}
\title{Write an editset into a mip representation}
\usage{
  as.mip(E, x = NULL, weight = NULL, M = 1e+07,
    epsilon = 0.001, prefix = "delta.", ...)
}
\arguments{
  \item{E}{an \code{link{editset}} or an object that is
  coerciable to an \code{editset}}

  \item{x}{record}

  \item{weight}{reliability weights}

  \item{M}{Constant that is used for allowing
  softconstraints}

  \item{epsilon}{Constant that is used for converting '<'
  into '<='}

  \item{prefix}{prefix for dummy variables}

  \item{...}{not used}
}
\value{
  a mip object containing al information for transforming
  it into an lp/mip problem
}
\description{
  Write an editset as a mip problem. Note that the
  objective function is empty
}

