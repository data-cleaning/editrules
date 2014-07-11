\name{violatedEdits}
\alias{as.data.frame.violatedEdits}
\alias{plot.violatedEdits}
\alias{summary.violatedEdits}
\alias{violatedEdits}
\alias{violatedEdits.character}
\alias{violatedEdits.editarray}
\alias{violatedEdits.editmatrix}
\alias{violatedEdits.editset}
\title{Check data against constraints}
\usage{
violatedEdits(E, dat, ...)

\method{violatedEdits}{character}(E, dat, name = NULL, ...)

\method{violatedEdits}{editmatrix}(E, dat, tol = 0, ...)

\method{violatedEdits}{editarray}(E, dat, datamodel = TRUE, ...)

\method{violatedEdits}{editset}(E, dat, datamodel = TRUE, ...)

\method{plot}{violatedEdits}(x, topn = min(10, ncol(x)), ...)

\method{summary}{violatedEdits}(object, E = NULL, minfreq = 1, ...)

\method{as.data.frame}{violatedEdits}(x, ...)
}
\arguments{
  \item{E}{\code{\link{character}} vector with
  constraintsm, \code{\link{editset}},
  \code{\link{editmatrix}} or \code{\link{editarray}}.}

  \item{dat}{\code{data.frame} with data that should be
  checked, if a named vector is supplied it will converted
  internally to a \code{data.frame}}

  \item{...}{further arguments that can be used by methods
  implementing this generic function}

  \item{name}{name of edits}

  \item{tol}{tolerance to check rules against.}

  \item{datamodel}{Also check against datamodel?}

  \item{x}{\code{violatedEdits} object.}

  \item{topn}{Top \code{n} edits to be plotted.}

  \item{object}{\code{violatedEdits} object}

  \item{minfreq}{minimum freq for edit to be printed}
}
\value{
An object of class \code{violatedEdits}, which is a logical
\code{nrow(dat)Xnedits(E)} matrix with an extra
\code{class} attribute for overloading purposes.
}
\description{
Determine which record violates which edits. Returns
\code{NA} when edits cannot be checked because of missing
values in the data.

\itemize{ \item{For rules of the form Ax == b |Ax - b| <=
tol is returned.} \item{For rules of the form Ax < b, Ax -
b < tol is returned.} \item{For rules of the form Ax <= b
Ax- b <= tol is returned.} } For numerical records, the
default tolerance is 0. When working with doubles, the
square root of machina accuracy is a resonable alternative
(\code{sqrt(.Machine\$double.eps)}). The editmatrix is
\cite{\link[=normalize]{normalized}} before checks are
performed.

Plot summary statistics on violatedEdits

as.data.frame violatedEdits
}
\note{
When summarizing an object of class \code{violatedEdits},
every empty value is counted as one edit violation when
counting violations per record.
}
\examples{
# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z"
                  , "x==z"
                  )
                )
                
dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)

ve <- violatedEdits(E,dat)

print(ve)
summary(ve, E)
plot(ve)

# An example with categorical data:

E <- editarray(expression(
    gender \%in\% c('male','female'),
    pregnant \%in\% c(TRUE, FALSE),
    if( gender == 'male' ) !pregnant
    )
)
print(E)

dat <- data.frame(
    gender=c('male','male','female','cylon'), 
    pregnant=c(TRUE,FALSE,TRUE,TRUE)
)

print(dat)
# Standard, the datamodel is checked as well,
violatedEdits(E,dat)

# but we may turn this of
violatedEdits(E,dat,datamodel=FALSE)


}
\seealso{
\code{\link{checkDatamodel}}
}

