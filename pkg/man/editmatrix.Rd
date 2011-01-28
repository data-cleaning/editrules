\name{editmatrix}
\alias{editmatrix}
\title{transforms a list of R equalities into a matrix with +1, -1 and 0 for each variable...}
\usage{editmatrix(file="", text, dat, addChecks=is.data.frame(dat))
}
\description{transforms a list of R equalities into a matrix with +1, -1 and 0 for each variable}
\details{i.e. x == y   results in  (x=-1, y=1, w=0, z=0)
and x == y + w results in (x=-1, y=1, w=1, z=0)}
\arguments{\item{file}{file containing R equalities}
\item{text}{character vector with R equalities}
\item{data}{data.frame containing the records to be checked, used for creating the matrix}
}

