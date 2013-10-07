\name{getH}
\alias{geth}
\alias{getH}
\title{Returns the derivation history of an edit matrix or array}
\usage{
  getH(E)

  geth(E)
}
\arguments{
  \item{E}{\code{\link{editmatrix}}}
}
\description{
  Function \code{\link{eliminate}} tracks the history of
  edits in a logical array H. H has nrow(E) rows and the
  number of columns is the number of edits in the
  \code{\link{editmatrix}} as it was first defined. If
  H[i,j1], H[i,j2],...,H[i,jn] are \code{TRUE}, then E[i,]
  is some (positive, linear) combination of original edits
  E[j1,], E[j2,],...,E[jn,]

  h records the number of variables eliminated from E by
  \code{\link{eliminate}}
}
\details{
  Attributes H and h are used to detect redundant derived
  edits.
}
\seealso{
  \code{\link{editmatrix}}, \code{\link{eliminate}}

  \code{\link{editmatrix}}, \code{\link{eliminate}}
}

