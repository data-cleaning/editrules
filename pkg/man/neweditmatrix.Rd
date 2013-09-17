\name{neweditmatrix}
\alias{neweditmatrix}
\title{Create an \code{editmatrix} object from its constituing attributes.}
\usage{
  neweditmatrix(A, ops, normalized = FALSE, ...)
}
\arguments{
  \item{A}{An augmented \code{matrix} of the form
  \code{A|b}}

  \item{ops}{a character vector with the comparison
  operator of every edit.}

  \item{normalized}{\code{logical} TRUE or FALSE}

  \item{...}{optional attributes}
}
\value{
  an S3 object of class \code{editmatrix}
}
\description{
  This function is for internal purposes, please use
  \code{\link{editmatrix}} for creating an editmatrix
  object.
}
\keyword{internal}

