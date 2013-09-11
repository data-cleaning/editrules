\name{separate}
\alias{separate}
\title{Separate an editset into its disconnected blocks and simplify}
\usage{
  separate(E)
}
\arguments{
  \item{E}{An \code{\link{editset}}}
}
\value{
  A \code{list} where each element is either an
  \code{\link{editmatrix}}, an \code{\link{editarray}} or
  an object of class \code{\link[=disjunct]{editlist}}
  which cannot be simplified further.
}
\description{
  The input edits are separated into disjunct blocks, and
  simplified to \code{editmatrix} or
  \code{\link{editarray}} where possible. Remaining
  \code{\link[=editset]{editsets}} are separated into
  \code{\link{disjunct}}
  \code{\link[=disjunct]{editlists}}.
}
\examples{


E <- editset(expression(
    x + y == z,
    2*u  + 0.5*v == 3*w,
    w >= 0,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A \%in\% letters[1:4],
    B \%in\% letters[1:4],
    C \%in\% c(TRUE,FALSE),
    D \%in\% letters[5:8],
    if ( A \%in\% c('a','b') ) y > 0,
    if ( A == 'c' ) B \%in\% letters[1:3],
    if ( !C == TRUE) D \%in\% c('e','f')
))

(L <- separate(E))

sapply(L,class)






}
\references{
  M. van der Loo and De Jonge, E. (2012). Manipulation of
  conditional restrictions and error localization with the
  editrules package. Discussion paper 2012xx, Statistics
  Netherlands, The Hague (included with the package).
}
\seealso{
  \code{\link{blocks}}, \code{\link{disjunct}},
  \code{\link{condition}}
}

