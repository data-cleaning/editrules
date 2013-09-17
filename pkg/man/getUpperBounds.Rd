\name{getUpperBounds}
\alias{getUpperBounds}
\title{Get upperbounds of edits, given the boundaries of all variables}
\usage{
  getUpperBounds(E, xlim)
}
\arguments{
  \item{E}{\code{editmatrix}}

  \item{xlim}{\code{matrix} with columns lower and upper,
  and rows are variables (in same order as E)}
}
\value{
  matrix with upperbounds per edit and a possible value
}
\description{
  Get upperbounds of edits, given the boundaries of all
  variables
}
\keyword{internal}

