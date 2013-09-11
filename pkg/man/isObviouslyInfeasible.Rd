\name{isObviouslyInfeasible}
\alias{isObviouslyInfeasible}
\alias{isObviouslyInfeasible.editarray}
\alias{isObviouslyInfeasible.editenv}
\alias{isObviouslyInfeasible.editlist}
\alias{isObviouslyInfeasible.editmatrix}
\alias{isObviouslyInfeasible.editset}
\title{Check for obvious contradictions in a set of edits}
\usage{
  isObviouslyInfeasible(E, ...)

  \method{isObviouslyInfeasible}{editmatrix} (E,
    tol = sqrt(.Machine$double.eps), ...)

  \method{isObviouslyInfeasible}{editarray} (E, ...)

  \method{isObviouslyInfeasible}{editset} (E, ...)

  \method{isObviouslyInfeasible}{editlist} (E, ...)

  \method{isObviouslyInfeasible}{editenv} (E, ...)
}
\arguments{
  \item{E}{An \code{\link{editset}},
  \code{\link{editmatrix}}, \code{\link{editarray}},
  \code{\link[=disjunct]{editlist}} or
  \code{\link[=disjunct]{editenv}}}

  \item{...}{Arguments to be passed to or from other
  methods.}

  \item{tol}{Tolerance for checking against zero.}
}
\value{
  A \code{logical} for objects of class
  \code{\link{editset}}, \code{\link{editarray}} or
  \code{\link{editmatrix}}.  A \code{logical} vector in the
  case of an \code{\link[=disjunct]{editlist}} or
  \code{\link[=disjunct]{editset}}.
}
\description{
  Obvious contradictions are edits of the form \eqn{1 < 0},
  or categorical edits defining that a record fails for any
  value combination If this function evaluates to
  \code{TRUE}, the set of edits is guaranteed infeasible.
  If it evaluates to \code{FALSE} this does not garuantee
  feasibility. See \code{\link{isFeasible}} for a complete
  test.
}
\seealso{
  \code{\link{isObviouslyRedundant}},
  \code{\link{isFeasible}}

  \code{\link{eliminate}} \code{\link{editmatrix}}
}

