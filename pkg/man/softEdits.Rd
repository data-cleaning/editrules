\name{softEdits}
\alias{softEdits}
\title{Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for
constructing a mip/lp problem.}
\usage{
softEdits(E, prefix = "delta.", ...)
}
\arguments{
  \item{E}{normalized \code{editmatrix}}

  \item{prefix}{\code{character} used for naming dummy
  variables in matrix.}
}
\description{
Derive editmatrix with soft constraints based on boundaries
of variables. This is a utility function that is used for
constructing a mip/lp problem.
}
\keyword{internal}

