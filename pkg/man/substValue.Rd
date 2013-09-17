\name{substValue}
\alias{substValue}
\alias{substValue.editarray}
\alias{substValue.editenv}
\alias{substValue.editlist}
\alias{substValue.editmatrix}
\alias{substValue.editset}
\title{Replace a variable by a value in a set of edits.}
\usage{
  substValue(E, var, value, ...)

  \method{substValue}{editmatrix} (E, var, value,
    reduce = FALSE, removeredundant = TRUE, ...)

  \method{substValue}{editarray} (E, var, value,
    reduce = FALSE, ...)

  \method{substValue}{editset} (E, var, value,
    simplify = TRUE, ...)

  \method{substValue}{editlist} (E, var, value, ...)

  \method{substValue}{editenv} (E, var, value, ...)
}
\arguments{
  \item{E}{\code{\link{editset}}, \code{\link{editmatrix}},
  \code{\link{editarray}},
  \code{\link[=disjunct]{editlist}} or
  \code{\link[=disjunct]{editenv}}}

  \item{var}{\code{character} with name(s) of variable(s)
  to substitute}

  \item{value}{vector with value(s) of variable(s)}

  \item{...}{arguments to be passed to or from other
  methods}

  \item{reduce}{\code{logical} should the result be
  simplified? For \code{\link{editmatrix}} this has the
  same effect as calling the function \code{\link{reduce}}.
  For \code{\link{editarray}}, the datamodel of the
  substituted variable is reduced to a single value, and
  the variable itself is not removed.}

  \item{removeredundant}{\code{logical}. Should empty rows
  be removed?}

  \item{simplify}{Simplify editset by moving logical edits
  containing a single numerical statement to the pure
  numerical part? (This is mostly for internal purposes and
  overwriting the default should normally not be necessary
  for package users).}
}
\value{
  \code{E}, with variables replaced by values
}
\description{
  Replace a variable by a value in a set of edits.
}
\note{
  At the moment, objects of class
  \code{\link[=disjunct]{editenv}} are converted to
  \code{list} prior to processing (so no performance is
  gained there) and reconverted afterwards.
}
\examples{


E <- editmatrix(expression(
    x + y == z,
    2*y < 10,
    3*x + 1.5*u < 7,
    z >= 0
    )
)

# single value
substValue(E,'z',10)
# multiple values
substValue(E,c('x','y'),c(1,3))
# remove substituted variable from edits
substValue(E,'z',10,reduce=TRUE)
# do not remove redundant row:
substValue(E,'z',10,removeredundant=FALSE)


# example with an editset
E <- editset(expression(
    x  + y == z,
    x >= 0,
    y >= 0,
    A \%in\% c('a1','a2'),
    B \%in\% c('b1','b2'),
    if ( x > 0 ) y > 0,
    if ( y > 0 ) x > 0,
    if ( A == 'a' ) B == 'b',
    if ( A == 'b' ) y > 3
    )
)

# substitute pure numerical variable
substValue(E,'z',10)
# substitute pure categorical variable
substValue(E,'A','a1')
# substitute variable appearing in logical constraints
substValue(E,'x',3)



}
\references{
  Value substitution is extensively described in the
  package vignettes.
}
\seealso{
  \code{\link{eliminate}}
}

