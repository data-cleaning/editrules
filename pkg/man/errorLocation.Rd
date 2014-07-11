\name{errorLocation}
\alias{errorLocation}
\alias{plot.errorLocation}
\alias{summary.errorLocation}
\title{The errorLocation object}
\usage{
\method{plot}{errorLocation}(x, topn = min(10, ncol(x$adapt)), ...)

\method{summary}{errorLocation}(object, ...)
}
\arguments{
  \item{x}{errorLocation object}

  \item{topn}{Number of variables to show in 'errors per
  variable plot'. Only the top-n are are shown.  By default
  the top-20 variables with the most errors are shown.}

  \item{...}{other arguments that will be transferred to
  \code{barplot}}

  \item{object}{an R object}
}
\description{
Object storing information on error locations in a dataset.

summary
}
\details{
The \code{errorlocation} objects consists of the following
slots wich can be accessed with the dollar operator, just
like with lists. Right now the only functions creating such
objects are \code{\link{localizeErrors}} and
\code{\link{checkDatamodel}}.

\itemize{ \item{\code{adapt} a \code{logical} array where
each row/column shows which record/variable should be
adapted.} \item{\code{status} A \code{data.frame} with the
same number of rows as \code{adapt}. It contains the
following columns \itemize{ \item{\code{weight} weight of
the found solution} \item{\code{degeneracy} number of
equivalent solutions found} \item{\code{user} user time
used to generate solution (as in \code{sys.time})}
\item{\code{system} system time used to generate solution
(as in \code{sys.time})} \item{\code{elapsed} elapsed time
used to generate solution (as in \code{sys.time})}
\item{\code{maxDurationExceeded} Was the maximum search
time reached?} \item{\code{memfail} Indicates whether a
branch was broken off due to memory allocation failure
(branch and bound only)} } } \item{\code{method} The error
localization method used, can be "mip", "localizer" or
"checkDatamodel".} \item{\code{call} The R calls to the
function generating the object.} \item{\code{user}
\code{character} user who generated the object.}
\item{\code{timestamp} \code{character} timestamp.} }

It is possible to \code{plot} objects of class
\code{errorLocation}. An overview containing three or four
graphs will be plotted in a new window. Axes in
scatterplots are set to logarithmic if their scales maxima
exceed 50.
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
\seealso{
\code{\link{localizeErrors}}, \code{\link{checkDatamodel}}
}

