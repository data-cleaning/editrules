\name{fcf.env}
\alias{fcf.env}
\title{Field code forest algorithm}
\usage{
  fcf.env(E, totreat, env)
}
\arguments{
  \item{E}{an editarray}

  \item{vars}{variable names still to be eliminated from E}

  \item{env}{an environment where all editmatrices will be
  stored}
}
\description{
  Workhorse function for \code{\link{generateEdits}}
}
\examples{


E <- editarray(expression(
    a \\\%in\\\% letters[1:4],
    b \\\%in\\\% letters[5:8],
    if ( a \\\%in\\\% c('a','b') ) b \\\%in\\\% c('e','f'),
    if ( a \\\%in\\\% c('c','d') ) b \\\%in\\\% c('h')
))

generateEdits(E)

## Not run
# load 60 edits (36 variables) from demonstration file
E <- editfile(system.file('script/bench/edits.R',package='editrules'),type='cat')
F <- generateEdits(E)

summary(F$edits)
F$nodes
F$dudation

## End(Not run)
}
\seealso{
  \code{\link{generateEdits}}, \code{\link{editarray}}
}
\keyword{internal}

