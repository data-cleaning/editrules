\name{c.editset}
\alias{as.character.editset}
\alias{as.data.frame.editset}
\alias{c.editset}
\alias{editset}
\alias{summary.editset}
\title{Read general edits}
\usage{
  \method{c}{editset} (...)

  editset(editrules, env = new.env())

  \method{as.character}{editset} (x, datamodel = TRUE,
    useIf = TRUE, dummies = FALSE, ...)

  \method{as.data.frame}{editset} (x, ...)

  \method{summary}{editset} (object, useBlocks = TRUE, ...)
}
\arguments{
  \item{editrules}{\code{character} vector,
  \code{expression} vector or \code{data.frame} (see
  details) containing edits.}

  \item{env}{environment to parse categorical edits in
  (normally, users need not specify this)}

  \item{x}{an \code{\link{editset}}}

  \item{datamodel}{include datamodel?}

  \item{useIf}{return vectorized version?}

  \item{dummies}{return datamodel for dummy variables?}

  \item{...}{arguments to be passed to or from other
  methods}

  \item{object}{an R object}

  \item{useBlocks}{\code{logical} Summarize each block?}
}
\value{
  \code{editset}: An object of class \code{editset}

  \code{as.data.frame}: a \code{data.frame} with columns
  'name' and 'edit'.
}
\description{
  An \code{editset} combines numerical (linear),
  categorical and conditional restrictions in a single
  object. Internally, it consists of two
  \code{\link[=editmatrix]{editmatrices}} and an
  \code{\link{editarray}}.
}
\details{
  The function \code{editset} converts a \code{character}
  or \code{expression} vector to an editset. Alternatively,
  a \code{data.frame} with a column called \code{edit} can
  be supplied. Function \code{\link{editfile}} reads edits
  from a free-form textfile.
}
\examples{

# edits can be read from a vector of expressions 
E <- editset(expression(
    if ( x > 0 ) y > 0,
    x + y == z,
    A \%in\% letters[1:2],
    B \%in\% letters[2:3],
    if ( A == 'a') B == 'b',
    if ( A == 'b') x >= 0,
    u + v == w,
    if ( u >= 0 ) w >= 0
))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')


## see also editfile
E <- editfile(system.file('script/edits/mixedits.R',package='editrules'))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')




# edits can be read from a vector of expressions 
E <- editset(expression(
    if ( x > 0 ) y > 0,
    x + y == z,
    A \%in\% letters[1:2],
    B \%in\% letters[2:3],
    if ( A == 'a') B == 'b',
    if ( A == 'b') x >= 0,
    u + v == w,
    if ( u >= 0 ) w >= 0
))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')


## see also editfile
E <- editfile(system.file('script/edits/mixedits.R',package='editrules'))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')



}
\seealso{
  \code{\link{editrules.plotting}},
  \code{\link{violatedEdits}},
  \code{\link{localizeErrors}}, \code{\link{getVars}},
  \code{\link{disjunct}}, \code{\link{eliminate}},
  \code{\link{substValue}}, \code{\link{isFeasible}},
  \code{\link{contains}}, \code{\link{is.editset}}
}

