\name{isObviouslyRedundant}
\alias{isObviouslyRedundant}
\alias{isObviouslyRedundant.editarray}
\alias{isObviouslyRedundant.editenv}
\alias{isObviouslyRedundant.editlist}
\alias{isObviouslyRedundant.editmatrix}
\alias{isObviouslyRedundant.editset}
\title{Find obvious redundancies in set of edits}
\usage{
  isObviouslyRedundant(E, duplicates = TRUE, ...)

  \method{isObviouslyRedundant}{editmatrix} (E,
    duplicates = TRUE, ...)

  \method{isObviouslyRedundant}{editarray} (E,
    duplicates = TRUE, ...)

  \method{isObviouslyRedundant}{editset} (E,
    duplicates = rep(TRUE, 2), ...)

  \method{isObviouslyRedundant}{editlist} (E,
    duplicates = rep(TRUE, 2), ...)

  \method{isObviouslyRedundant}{editenv} (E,
    duplicates = rep(TRUE, 2), ...)
}
\arguments{
  \item{E}{An \code{\link{editset}},
  \code{\link{editmatrix}}, \code{\link{editarray}},
  \code{\link[=disjunct]{editlist}} or
  \code{\link[=disjunct]{editenv}}}

  \item{duplicates}{\code{logical}: check for duplicate
  edits? For an \code{\link{editset}},
  \code{\link[=duplicated]{editlist}} or
  \code{\link[=duplicated]{editenv}} this should be a
  logical 2-vector indicating which of the numerical or
  categorical edits should be checked for duplicates.}

  \item{...}{parameters to be passed to or from other
  methods.}
}
\value{
  logical vector indicating which edits are (obviously)
  redundant
}
\description{
  Detect simple redundancies such as duplicates or edits of
  the form \code{0 < 1} or \code{0 == 0}. For categorical
  edits, simple redundancies are edits that define an empty
  subregion of the space of all possible records (no record
  can ever be contained in such a region).
}
\seealso{
  \code{\link{isObviouslyInfeasible}},
  \code{\link{isSubset}}
}

