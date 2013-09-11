\name{contains}
\alias{contains}
\alias{contains.cateditmatrix}
\alias{contains.editarray}
\alias{contains.editmatrix}
\alias{contains.editset}
\alias{contains.matrix}
\title{Determine which edits contain which variable(s)}
\usage{
  contains(E, var = NULL, ...)

  \method{contains}{matrix} (E, var = NULL,
    tol = sqrt(.Machine$double.eps), ...)

  \method{contains}{editmatrix} (E, var = NULL,
    tol = sqrt(.Machine$double.eps), ...)

  \method{contains}{editarray} (E, var = NULL, ...)

  \method{contains}{editset} (E, var = NULL, ...)

  \method{contains}{editset} (E, var = NULL, ...)
}
\arguments{
  \item{E}{\code{\link{editarray}},
  \code{\link{editmatrix}}, \code{\link{editset}}, or
  \code{matrix}}

  \item{var}{\code{character}, names of a categorical
  variables in \code{E}. If var=NULL, all variables are
  treated.}

  \item{...}{arguments to be passed to other methods}

  \item{tol}{tolerance to check zero-entries}
}
\value{
  \code{logical} vector of length nrow(E), TRUE for edits
  containing \code{var}
}
\description{
  For an \code{\link{editmatrix}}, variables with
  coefficients smaller than \code{tol} are considered not
  to be contained in an edit.

  matrix method for contains

  editmatrix method for contains

  contains method for editarray

  contains method for cateditmatrix

  contains method for editset
}
\keyword{internal}

