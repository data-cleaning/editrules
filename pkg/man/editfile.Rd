\name{editfile}
\alias{editfile}
\title{Read edits edits from free-form textfile}
\usage{
  editfile(file, type = c("all", "num", "cat", "mix"), ...)
}
\arguments{
  \item{file}{name of text file to read in}

  \item{type}{type of edits to extract. Currently, only
  'num' (numerical), 'cat' (categorical) and 'all' are
  implemented.}

  \item{...}{extra parameters that are currently ignored}
}
\value{
  \code{\link{editset}} with all edits if \code{type=all},
  \code{\link{editarray}} if \code{type='cat'},
  \code{\link{editmatrix}} if \code{type='num'},
  \code{\link{editset}} with conditional edits if
  \code{type='mix'}.  If the return value is a \code{list},
  the elements are named \code{numedits} and
  \code{catedits}.
}
\description{
  This utility function allows for free editrule definition
  in a file. One can extract only the numerical
  (\code{type='num'}), only the categorical
  (\code{type='cat'}) or all edits (default) in which case
  an \code{\link{editset}} is returned. The function first
  parses all assignments in the file, so it is possible to
  compute or read a list of categories defining a datamodel
  for example.
}

