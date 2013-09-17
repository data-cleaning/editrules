\name{reduce}
\alias{reduce}
\alias{reduce.editarray}
\alias{reduce.editmatrix}
\alias{reduce.editset}
\title{Remove redundant variables and edits.}
\usage{
  reduce(E, ...)

  \method{reduce}{editmatrix} (E,
    tol = sqrt(.Machine$double.eps), ...)

  \method{reduce}{editarray} (E, ...)

  \method{reduce}{editset} (E, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{...}{arguments to pass to other methods}

  \item{tol}{elements of \code{E} with absolute value <
  \code{tol} are considered 0.}
}
\description{
  Remove variables which are not contained in any edit and
  remove edits which are
  \code{\link[=isObviouslyRedundant]{obviously redundant}}.
}
\seealso{
  \code{\link{contains}}, \code{\link{eliminate}},
  \code{\link{substValue}}
}

