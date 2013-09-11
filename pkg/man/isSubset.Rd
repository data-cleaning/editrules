\name{isSubset}
\alias{isSubset}
\title{Check which edits are dominated by other ones.}
\usage{
  isSubset(E)
}
\arguments{
  \item{E}{\code{\link{editarray}}}
}
\value{
  \code{logical} vector indicating if an edit is a subset
  of at least one other edit.
}
\description{
  An edit defines a subregion of the space of all possible
  value combinations of a record. Records in this region
  are interpreted as invalid. An edit rule which defines a
  region equal to or contained in the region defined by
  another edit is redundant. (In data editing literature,
  this is often referred to as a \emph{domination}
  relation.)
}

