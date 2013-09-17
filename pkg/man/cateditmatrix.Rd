\name{cateditmatrix}
\alias{cateditmatrix}
\title{Create an editmatrix with categorical variables}
\usage{
  cateditmatrix(x, sep = ":", env = parent.frame())
}
\arguments{
  \item{x}{\code{character or expression} with categorical
  edits}

  \item{sep}{seperator to be used in naming categories
  within variables.}

  \item{env}{environment in which \code{x} will be
  evaluated.}
}
\value{
  cateditmatrix object, which is a specialized
  \code{\link{editmatrix}}
}
\description{
  \code{cateditmatrix} is an alternative representation of
  a categorial edit. The default representation in
  editrules is \code{\link{editarray}}, but cateditmatrix
  is useful for transforming and solving categorical edit
  into a mixed integer programming problem
}
\keyword{internal}

