\name{as.editmatrix}
\alias{as.editmatrix}
\title{Coerce a matrix to an edit matrix.}
\usage{
  as.editmatrix(A, b = numeric(nrow(A)),
    ops = rep("==", nrow(A)), ...)
}
\arguments{
  \item{A}{matrix to be transformed into an
  \code{\link{editmatrix}}.}

  \item{b}{Constant, a \code{numeric} of
  \code{length(nrow(x))}, defaults to 0}

  \item{ops}{Operators, \code{character} of
  \code{length(nrow(x))} with the equality operators,
  defaults to "=="}

  \item{...}{further attributes that will be attached to
  the resulting editmatrix}
}
\value{
  an object of class \code{editmatrix}.
}
\description{
  \code{as.editmatrix} interpretes the matrix as an
  editmatrix. The columns of the matrix are the variables
  and the rows are the edit rules (contraints).
}
\details{
  If only argument \code{x} is given (the default), the
  resulting editmatrix is of the form \eqn{Ax=0}. This can
  be influenced by using the parameters \code{b} and
  \code{ops}.
}
\seealso{
  \code{\link{editmatrix}}
}

