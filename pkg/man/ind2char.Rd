\name{ind2char}
\alias{ind2char}
\title{Derive textual representation from (partial) indices}
\usage{
  ind2char(ivd, ind = ivd, invert = logical(length(ivd)),
    useEqual = TRUE)
}
\arguments{
  \item{ind}{a \code{list}, usually a (part of) the 'ind'
  attribute of an editarray}

  \item{invert}{\code{logical} vector of lenght
  length(ind)}
}
\value{
  For every entry in \code{ind}, a character vector is
  returned where each entry is a statement of the form
  \code{"<var> \%in\% c('<cat1>',...,'<catN>')" or "<var>
  == '<catt>'"} if invert==TRUE. If invert==FALSE, the
  negation of the above statements is returned.
}
\description{
  Derive textual representation from (partial) indices
}
\keyword{internal}

