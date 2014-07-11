\name{localize}
\alias{localize}
\title{Workhorse function for localizeErrors}
\usage{
localize(E, dat, verbose, pretext = "Processing", call = sys.call(), weight,
  maxduration, method = c("bb", "mip", "localizer"), retrieve, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{dat}{\code{data.frame}}

  \item{verbose}{\code{logical} print progress report
  during run?}

  \item{pretext}{\code{character} text to print before
  progress report}

  \item{weight}{a \code{nrow(dat)xncol(dat)} array of
  weights}

  \item{method}{should branch and bound ("bb") or mix
  integer programming ("mip") be used?}

  \item{retrieve}{return \code{best} or \code{first} found
  solution (bb method only)}

  \item{maxduration}{max time for searchBest()}
}
\description{
Workhorse function for localizeErrors
}
\keyword{internal}

