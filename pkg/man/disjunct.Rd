\name{disjunct}
\alias{disjunct}
\title{Decouple a set of conditional edits}
\usage{
  disjunct(E, type = c("list", "env"))
}
\arguments{
  \item{E}{Object of class \code{\link{editset}}}

  \item{type}{Return type: \code{list} (default) for
  \code{editlist}, \code{env} for \code{editenv}.}
}
\value{
  An object of class \code{editlist} (\code{editenv}),
  which is nothing more than a \code{list}
  (\code{environment}) of \code{editsets} with a class
  attribute. Each element has an attribute 'condition'
  showing which conditions were assumed to derive the
  editset.
}
\description{
  An editset is transformed to a list of
  \code{\link{editset}s} which do not contain any
  conditional numeric/categorical edits anymore. Each
  \code{\link{editset}} gains an attribute
  \code{\link{condition}}, which holds the series of
  assumptions made to decouple the original edits. This
  attribute will be printed when not \code{NULL}. Warning:
  this may be slow for large, highly entangled sets of
  edits.
}
\examples{


E <- editset(expression(
    x + y == z,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A \%in\% letters[1:4],
    B \%in\% letters[1:4],
    if (A \%in\% c('a','b')) y > 0,
    if (A == 'c' ) B \%in\% letters[1:3]
))

disjunct(E)




}
\seealso{
  \code{\link{separate}}, \code{\link{condition}},
  \code{\link{blocks}}
}

