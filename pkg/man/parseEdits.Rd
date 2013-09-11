\name{parseEdits}
\alias{parseEdits}
\title{Parse a character vector of edits}
\usage{
  parseEdits(E, type = c("all", "num", "cat", "mix"))
}
\arguments{
  \item{E}{\code{character}}

  \item{type}{optional filter for type of edit, may be
  \code{"all"}, \code{"num"} \code{"cat"} or \code{"mix"}}
}
\value{
  The edits in \code{E} parsed to R expressions.
}
\description{
  This function wraps the native \code{parse} function in a
  \code{tryCatch}. The function is \code{editrules}
  internal. It tries to give a meaningfull error message
  when parsing fails for some reason.
}
\keyword{internal}

