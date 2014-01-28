\name{localizeErrors}
\alias{localizeErrors}
\title{Localize errors on records in a data.frame.}
\usage{
  localizeErrors(E, dat, verbose = FALSE,
    weight = rep(1, ncol(dat)), maxduration = 600,
    method = c("bb", "mip", "localizer"), useBlocks = TRUE,
    retrieve = c("best", "first"), ...)
}
\arguments{
  \item{E}{an object of class \code{\link{editset}}
  \code{\link{editmatrix}} or \code{\link{editarray}}}

  \item{dat}{a \code{data.frame} with variables in E.}

  \item{useBlocks}{\code{DEPRECATED}. Process error
  localization seperatly for independent blocks in \code{E}
  (always \code{TRUE})?}

  \item{verbose}{print progress to screen?}

  \item{weight}{Vector of positive weights for every
  variable in \code{dat}, or an \code{array} or
  \code{data.frame} of weights with the same dimensions as
  \code{dat}.}

  \item{method}{should errorlocalizer ("bb") or mix integer
  programming ("mip") be used?}

  \item{retrieve}{Return the first found solution or the
  best solution? ("bb" method only).}

  \item{maxduration}{maximum time for \code{$searchBest()}
  to find the best solution for a single record.}

  \item{...}{Further options to be passed to
  \code{\link{errorLocalizer}} or
  \code{\link{errorLocalizer_mip}}. Specifically, when
  \code{method='mip'}, the parameter \code{lpcontrol} is a
  list of options passed to \code{lpSolveAPI}.}
}
\value{
  an object of class \code{\link{errorLocation}}
}
\description{
  For each record in a \code{data.frame}, the least
  (weighted) number of fields is determined which can be
  adapted or imputed so that no edit in \code{E} is
  violated. Anymore.
}
\details{
  For performance purposes, the edits are split in
  independent \code{\link{blocks}} which are processed
  separately. Also, a quick vectorized check with
  \code{\link{checkDatamodel}} is performed first to
  exclude variables violating their one-dimensional bounds
  from further calculations.

  By default, all weights are set equal to one (each
  variable is considered equally reliable). If a vector of
  weights is passed, the weights are assumed to be in the
  same order as the columns of \code{dat}. By passing an
  array of weights (of same dimensions as \code{dat})
  separate weights can be specified for each record.

  In general, the solution to an error localization problem
  need not be unique, especially when no weights are
  defined. In such cases, \code{localizeErrors} chooses a
  solution randomly. See \code{\link{errorLocalizer}} for
  more control options.

  Error localization can be performed by the Branch and
  Bound method of De Waal (2003) (option
  \code{method="localizer"}, the default) or by rewriting
  the problem as a mixed-integer programming (MIP) problem
  (\code{method="mip"}) which is parsed to the
  \code{lpsolve} library. The former case uses
  \code{\link{errorLocalizer}} and is very reliable in
  terms of numerical stability, but may be slower in some
  cases (see note below). The MIP approach is much faster,
  but requires that upper and lower bounds are set on each
  numerical variable. Sensible bounds are derived
  automatically (see the vignette on error localization as
  MIP), but could cause instabilities in very rare cases.
}
\note{
  The Branch and Bound method is potentially slow for large
  sets of connected edits, especially when conditional
  edits are involved. Consider using \code{method="mip"} in
  such cases. The run-time of the B&B algorithm is related
  to the number of uquivalent solutions, so setting
  different weights (reducing the number of unique
  solutions) mey reduce computation time as well.
}
\examples{

# an editmatrix and some data:
E <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0"))

dat <- data.frame(
    x = c(1,-1,1),
    y = c(-1,1,1),
    z = c(2,0,2))

# localize all errors in the data
err <- localizeErrors(E,dat)

summary(err)

# what has to be adapted:
err$adapt
# weight, number of equivalent solutions, timings,
err$status


## Not run

# Demonstration of verbose processing
# construct 2-block editmatrix
F <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0",
    "w > 10"))
# Using 'dat' as defined above, generate some extra records
dd <- dat
for ( i in 1:5 ) dd <- rbind(dd,dd)
dd$w <- sample(12,nrow(dd),replace=TRUE)

# localize errors verbosely
(err <- localizeErrors(F,dd,verbose=TRUE))

# printing is cut off, use summary for an overview
summary(err)

# or plot (not very informative in this artificial example)
plot(err)

## End(Not run)

for ( d in dir("../pkg/R",full.names=TRUE)) dmp <- source(d)
# Example with different weights for each record
E <- editmatrix('x + y == z')
dat <- data.frame(
    x = c(1,1),
    y = c(1,1),
    z = c(1,1))

# At equal weights, both records have three solutions (degeneracy): adapt x, y
# or z:
localizeErrors(E,dat)$status

# Set different weights per record (lower weight means lower reliability):
w <- matrix(c(
    1,2,2,
    2,2,1),nrow=2,byrow=TRUE)

localizeErrors(E,dat,weight=w)


# an example with categorical variables
E <- editarray(expression(
    age \%in\% c('under aged','adult'),
    maritalStatus \%in\% c('unmarried','married','widowed','divorced'),
    positionInHousehold \%in\% c('marriage partner', 'child', 'other'),
    if( age == 'under aged' ) maritalStatus == 'unmarried',
    if( maritalStatus \%in\% c('married','widowed','divorced')) 
      !positionInHousehold \%in\% c('marriage partner','child')
    )
)
E

#
dat <- data.frame(
    age = c('under aged','adult','adult' ),
    maritalStatus=c('married','unmarried','widowed' ), 
    positionInHousehold=c('child','other','marriage partner')
)
dat
localizeErrors(E,dat)
# the last record of dat has 2 degenerate solutions. Running  the last command
# a few times demonstrates that one of those solutions is chosen at random.

# Increasing the weight of  'positionInHousehold' for example, makes the best
# solution unique again
localizeErrors(E,dat,weight=c(1,1,2))


# an example with mixed data:

E <- editset(expression(
    x + y == z,
    2*u  + 0.5*v == 3*w,
    w >= 0,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A \%in\% letters[1:4],
    B \%in\% letters[1:4],
    C \%in\% c(TRUE,FALSE),
    D \%in\% letters[5:8],
    if ( A \%in\% c('a','b') ) y > 0,
    if ( A == 'c' ) B \%in\% letters[1:3],
    if ( !C == TRUE) D \%in\% c('e','f')
))

set.seed(1)
dat <- data.frame(
    x = sample(-1:8),
    y = sample(-1:8),
    z = sample(10),
    u = sample(-1:8),
    v = sample(-1:8),
    w = sample(10),
    A = sample(letters[1:4],10,replace=TRUE),
    B = sample(letters[1:4],10,replace=TRUE),
    C = sample(c(TRUE,FALSE),10,replace=TRUE),
    D = sample(letters[5:9],10,replace=TRUE),
    stringsAsFactors=FALSE
)

(el <-localizeErrors(E,dat,verbose=TRUE))





}
\references{
  T. De Waal (2003) Processing of Erroneous and Unsafe
  Data. PhD thesis, University of Rotterdam.

  E. De Jonge and Van der Loo, M. (2012) Error localization
  as a mixed-integer program in editrules (included with
  the package)

  lp_solve and Kjell Konis. (2011). lpSolveAPI: R Interface
  for lp_solve version 5.5.2.0. R package version
  5.5.2.0-5.  http://CRAN.R-project.org/package=lpSolveAPI
}
\seealso{
  \code{\link{errorLocalizer}}
}

