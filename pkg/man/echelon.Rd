\name{echelon}
\alias{echelon}
\alias{echelon.editmatrix}
\alias{echelon.editset}
\alias{echelon.matrix}
\title{Bring an (edit) matrix to reduced row echelon form.}
\usage{
  echelon(E, ...)

  \method{echelon}{editmatrix} (E, ...)

  \method{echelon}{matrix} (E,
    tol = sqrt(.Machine$double.eps), ...)

  \method{echelon}{editset} (E, ...)
}
\arguments{
  \item{E}{a matrix or editmatrix}

  \item{...}{options to pass on to further methods.}

  \item{tol}{tolerance that will be used to determine if a
  coefficient equals zero.}
}
\description{
  If \code{E} is a matrix, a matrix in reduced row echelon
  form is returned. If \code{E} is an
  \code{\link{editmatrix}} the equality part of \code{E} is
  transformed to reduced row echelon form. For an
  \code{\link{editset}}, the numerical part is transformed
  to reduced row echelon form.
}
\seealso{
  \code{\link{eliminate}}, \code{\link{substValue}}
}

