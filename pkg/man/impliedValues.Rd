\name{impliedValues}
\alias{impliedValues}
\alias{impliedValues.editmatrix}
\title{Retrieve values stricktly implied by rules}
\usage{
impliedValues(E, ...)

\method{impliedValues}{editmatrix}(E, tol = sqrt(.Machine$double.eps), ...)
}
\arguments{
  \item{E}{editmatrix}

  \item{tol}{Maximum deviation for two values to be
  considered equal.}

  \item{...}{Currently unused}
}
\value{
Numeric vector, whose names are variable names and values
are unique values implied by the rules.
}
\description{
Retrieve values stricktly implied by rules

Detects cases where two inequalities imply an equality,
e.g. \eqn{x\leq 0} and \eqn{x\geq0} implies \eqn{x=0}. Also
detects straight equalities, e.g. \eqn{x==0} implies
\eqn{x=0}. Such cases arise frequently when manipulating
edits by value subsitution or variable elimination. The
function recursively detects equalities and combined
inequalities that imply fixed values, substitutes those
fixed values and looks for new implied values until no new
values are found.
}
\seealso{
\code{\link{reduce}}, \code{\link{substValue}},
\code{\link{eliminate}}
}

