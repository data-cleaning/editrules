\name{simplify}
\alias{simplify}
\title{Simplify logical mixed edits in an editset}
\usage{
  simplify(E, m = NULL)
}
\arguments{
  \item{E}{an editset}

  \item{m}{\code{contains(E)}. Speeds up calculation if you
  have one handy.}
}
\description{
  Logical edits consisting of a single numerical statement
  are inverted and added to the \code{editmatrix}
  (\code{\$num}) of the editset.
}
\keyword{internal}

