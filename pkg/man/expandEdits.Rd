\name{expandEdits}
\alias{expandEdits}
\title{Expand an edit expression}
\usage{
  expandEdits(s, prefix = "_", useSum = TRUE,
    asExpression = is.language(s), env = parent.frame(),
    ...)
}
\arguments{
  \item{s}{edit expression, can be a \code{character} or
  \code{expression} vector}

  \item{prefix}{prefix for variables to be expanded}

  \item{useSum}{if \code{TRUE} sum expressions will be
  expanded}

  \item{asExpression}{if \code{TRUE} an
  \code{\link{expression}} will be returned in stead of a
  \code{character}}

  \item{env}{enviroment that will be used to find variables
  to be expanded}

  \item{...}{variables used in the expansion}
}
\value{
  \code{character} or \code{expression} vector with
  expanded expressions
}
\description{
  Often many numeric variables have the same constraints.
  \code{expandEdits} is a utility function to define edits
  for multiple variables. See the examples for the syntax.
}
\keyword{internal}

