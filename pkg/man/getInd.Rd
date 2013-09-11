\name{getInd}
\alias{getInd}
\title{get index list from editmatrix}
\usage{
  getInd(E)
}
\arguments{
  \item{E}{\code{\link{editarray}}}
}
\value{
  named list, indexing category levels in the editarray
  (columns)
}
\description{
  The 'ind' attribute is a named list of named integer
  vectors. The list names are the variable names. The
  vectors in the list index the columns in the editarray
  associated with the variables. The names of the vectors
  are the names of the columns of the editarray.
}
\keyword{internal}

