\name{[.editmatrix}
\alias{[.cateditmatrix}
\alias{[.editarray}
\alias{[.editlist}
\alias{[.editmatrix}
\alias{[.editset}
\title{Row index operator for \code{editmatrix}}
\usage{
  \method{[}{editmatrix} (x, i, j, ...)

  \method{[}{editarray} (x, i, j, ...)

  \method{[}{editarray} (x, i, j, ...)

  \method{[}{editset} (x, i, j, ...)

  \method{[}{editlist} (x, i, j, ...)
}
\arguments{
  \item{x}{an object of class \code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{i}{the row index in the edit matrix (numeric,
  logical or rowname)}

  \item{j}{the column index in the edit matrix}

  \item{...}{arguments to be passed to other methods.
  Currently ignored.}
}
\description{
  Use this operator to select edits from an editmatrix or
  editarray object.

  Row index operator for \code{cateditmatrix}

  Row index operator for \code{editarray}

  Row index operator for \code{editset} Note: the datamodel
  is not changed

  Index operator for \code{editlist}
}
\keyword{internal}

