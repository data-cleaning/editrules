\name{checkDatamodel}
\alias{checkDatamodel}
\title{Check data against a datamodel}
\usage{
  checkDatamodel(E, dat, weight = rep(1, ncol(dat)), ...)
}
\arguments{
  \item{E}{an object of class \code{\link{editset}},
  \code{\link{editarray}}, or \code{\link{editmatrix}}}

  \item{dat}{a \code{data.frame}}

  \item{weight}{vector of weigths for every variable of
  \code{dat} or an array of weight of the same dimensions
  as \code{dat}.}

  \item{...}{arguments to be passed to or from other
  methods}
}
\value{
  An object of class \code{\link{errorLocation}}.
}
\description{
  Categorical variables in \code{dat} which also occur in
  \code{E} are checked against the datamodel for those
  variables. Numerical variables are checked against edits
  in \code{E} that contain only a single variable (e.g.
  \eqn{x > 0}). Values violating such edits as well as
  empty values are set to adapt.
}
\seealso{
  \code{\link{errorLocation}},
  \code{\link{localizeErrors}}.
}

