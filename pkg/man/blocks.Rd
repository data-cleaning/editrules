\name{blocks}
\alias{blockIndex}
\alias{blocks}
\title{Decompose a matrix or edits into independent blocks}
\usage{
  blocks(M)

  blockIndex(D)
}
\arguments{
  \item{M}{\code{matrix}, \code{\link{editmatrix}},
  editarray or editset to be decomposed into independent
  blocks}

  \item{D}{matrix of type \code{logical}}
}
\value{
  list of independent subobjects of \code{M}.

  \code{list} of row indices in \code{D} indicating
  independent blocks.  Empty rows (i.e. every column
  \code{FALSE}) are ignored.
}
\description{
  \code{blocks} returns a \code{list} of independent blocks
  \eqn{M_i} such that \eqn{M=M_1\oplus
  M_2\oplus\cdots\oplus M_n}.

  \code{blockIndex} returns a list of row indices in a
  \code{logical} matrix \code{D} designating independent
  blocks.
}
\examples{
# three seperate blocks
E <- editmatrix( expression( 
  x1 + x2 == x3,
  x3 + x4 == x5,
  x5 + x6 == x7,
  y1 + y2 == y3,
  z1 + z2 == z3
))
blocks(E)

# four seperate blocks
E <- editmatrix(expression( 
  x1 + x2 == x3,
  x3 + x4 == x5,
  x8 + x6 == x7,
  y1 + y2 == y3,
  z1 + z2 == z3
))
blocks(E)

# two categorical blocks
E <- editarray(expression(
 x \%in\% c('a','b','c'),
 y \%in\% c('d','e'),
 z \%in\% c('f','g'),
 u \%in\% c('w','t'),
 if ( x == 'a') y != 'd',
 if ( z == 'f') u != 'w'
))
blocks(E)


}

