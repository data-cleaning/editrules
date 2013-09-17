\name{errorLocalizer_mip}
\alias{errorLocalizer_mip}
\title{Localize errors using a MIP approach.}
\usage{
  errorLocalizer_mip(E, x, weight = rep(1, length(x)),
    maxduration = 600L, verbose = "neutral", ...)
}
\arguments{
  \item{E}{an \code{\link{editset}},
  \code{\link{editmatrix}}, or \code{\link{editarray}}}

  \item{x}{named \code{numeric} with data}

  \item{weight}{\code{numeric} with weights}

  \item{maxduration}{number of seconds that is spent on
  finding a solution}

  \item{verbose}{verbosity argument that will be passed on
  to \code{solve} lpSolveAPI}

  \item{...}{other arguments that will be passed on to
  \code{solve}.}
}
\value{
  list with solution weight \code{w}, \code{logical}
  \code{adapt} stating what to adapt, \code{x_feasible} and
  the lp problem (an \code{lpExtPtr} object)
}
\description{
  Localize errors using a MIP approach.
}
\section{Details}{
  \code{errorLocalizer_mip} uses \code{E} and \code{x} to
  define a mixed integer problem and solves this problem
  using \code{lpSolveApi}. This function can be much faster
  then \code{\link{errorLocalizer}} but does not return the
  degeneracy of a solution. However it does return an
  bonus: \code{x_feasible}, a feasible solution.
}

\section{Note}{
  If the maximim absolute value of \eqn{x\geq 10^9} 1E9, it
  is pre-scaled with a factor of \eqn{\sqrt{\max(|x|)}}.
}
\references{
  E. De Jonge and Van der Loo, M. (2012) Error localization
  as a mixed-integer program in editrules (included with
  the package)

  lp_solve and Kjell Konis. (2011). lpSolveAPI: R Interface
  for lp_solve version 5.5.2.0. R package version
  5.5.2.0-5.  http://CRAN.R-project.org/package=lpSolveAPI
}
\seealso{
  \code{\link{localizeErrors}},
  \code{\link{errorLocalizer}}, \code{\link{errorLocation}}
}

