\name{editType}
\alias{editType}
\title{Determine edittypes in editset based on 'contains(E)'}
\usage{
  editType(E, m = NULL)
}
\arguments{
  \item{E}{editset}

  \item{m}{if you happen to have \code{contains(E)} handy,
  it needs not be recalculated.}
}
\description{
  Determines edittypes based on the variables they contain
  (not on names of edits).
}
\seealso{
  \code{\link{contains}}
}

