\name{isFeasible}
\alias{isFeasible}
\title{Check consistency of set of edits}
\usage{
isFeasible(E, warn = FALSE)
}
\arguments{
  \item{E}{an \code{\link{editmatrix}},
  \code{\link{editarray}} or \code{\link{editset}}}

  \item{warn}{logical: should a warning be emitted when
  system is infeasible?}
}
\value{
TRUE or FALSE
}
\description{
When variables are \code{\link[=eliminate]{eliminated}} one
by one from a set of edits, eventually either no edits are
left or an \code{\link[=isObviouslyInfeasible]{obvious
contradiction}} is encountered. In the case no records can
obey all edits in the set which is therefore
\code{inFeasible}.
}
\note{
This function can potentially take a long time to complete,
especially when many connected (conditional) edits are
present. Consider using \code{\link{blocks}} to check
feasibility of indendent blocks.
}
\seealso{
\code{\link{isObviouslyInfeasible}},
\code{\link{isObviouslyRedundant}}
}

