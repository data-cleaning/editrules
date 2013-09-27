\name{getVars}
\alias{getVars}
\alias{getVars.editset}
\alias{getVars.NULL}
\title{get names of variables in a set of edits}
\usage{
  getVars(E, ...)

  \method{getVars}{editset} (E,
    type = c("all", "num", "cat", "mix", "dummy"), ...)

  \method{getVars}{NULL} (E, ...)
}
\arguments{
  \item{E}{\code{\link{editset}}, \code{\link{editmatrix}},
  or \code{\link{editarray}}}

  \item{...}{Arguments to be passed to or from other
  methods}

  \item{type}{(editset- or list only) select which
  variables to return. \code{all} means all (except
  dummies), \code{num} means all numericals, \code{cat}
  means all categoricals, \code{mix} means those numericals
  appearing in a logical constraint and \code{dummy} means
  dummy variables connecting the logical with numerical
  constraints.}
}
\value{
  \code{character} vector with the names of the variables.
}
\description{
  get names of variables in a set of edits
}
\examples{

E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
getVars(E)

E <- editarray(c(
    "gender \\\%in\\\% c('male','female')",
    "pregnant \\\%in\\\% c(TRUE, FALSE)",
    "if( gender == 'male' ) pregnant == FALSE"
    )
)

getVars(E)

}
\seealso{
  \code{\link{getA}}, \code{\link{getb}},
  \code{\link{getAb}}, \code{\link{getOps}}
}

