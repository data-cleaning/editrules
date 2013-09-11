\name{parseMix}
\alias{parseMix}
\title{Parse a mixed edit}
\usage{
  parseMix(e, editname = "", numid = 0, negate = TRUE)
}
\arguments{
  \item{e}{expression to be parsed}

  \item{numid}{starting number for dummy name generation}
}
\value{
  list with categorical expression (\code{cat}), which is
  normalized, a numerical expression (code{nums}) and a
  negated version of this expression (code{negNums})
}
\description{
  parseMix replaces all numerical edits with a generated
  dummy boolean variable and returns the resulting
  categorical edit plus the list of found of numerical
  edits. These expressions should be handled further by
  \code{parseCat} and \code{parseNum}.
}
\keyword{internal}

