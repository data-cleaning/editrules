\name{softEdits.editmatrix}
\alias{softEdits.editmatrix}
\title{Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for
constructing a mip/lp problem.}
\usage{
  softEdits.editmatrix(E, prefix = "delta.", postfix = "",
    M = 1e+07, ...)
}
\arguments{
  \item{E}{normalized \code{editmatrix}}

  \item{prefix}{\code{character} used for naming dummy
  variables in matrix.}
}
\description{
  Derive editmatrix with soft constraints based on
  boundaries of variables. This is a utility function that
  is used for constructing a mip/lp problem.
}
\keyword{internal}

