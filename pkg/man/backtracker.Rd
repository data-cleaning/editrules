\name{backtracker}
\alias{backtracker}
\alias{choicepoint}
\title{Backtracker: a flexible and generic binary search program}
\usage{
  backtracker(isSolution, choiceLeft, choiceRight,
    list = NULL, maxdepth = Inf, maxduration = Inf, ...)
}
\arguments{
  \item{isSolution}{\code{expression} that should evaluate
  to \code{TRUE} when a solution is found.}

  \item{choiceLeft}{\code{expression} that will be
  evaluated for a left node}

  \item{choiceRight}{\code{expression} that will be
  evaluated for a right node}

  \item{list}{\code{list} with variables that will be added
  to the search environment}

  \item{maxdepth}{\code{integer} maximum depth of the
  search tree}

  \item{maxduration}{\code{integer} Default maximum search
  time for \code{$searchNext()} and \code{$searchAll()}}

  \item{...}{named variables that will be added to the
  search environment}
}
\value{
  backtracker object, see Methods for a description of the
  methods
}
\description{
  \code{backtracker} creates a binary search program that
  can be started by calling the \code{$searchNext} function
  It walks a binary tree depth first. For all left nodes
  \code{choiceLeft} is evaluated, for all right nodes
  \code{choiceRight} is evaluated. A solution is found if
  \code{isSolution} evaluates to \code{TRUE}. In that case
  \code{$searchNext} will return all variables in the
  search environment in a \code{list} If \code{isSolution}
  evaluates to NULL it will continue to search deaper. If
  \code{isSolution} evaluates to \code{FALSE} it stops at
  the current node and goes up the next search node
}
\details{
  \subsection{Methods}{ \describe{
  \item{\code{$searchNext(..., VERBOSE=FALSE)}}{Search next
  solution, can be called repeatedly until there is no
  solution left. Named variables will be added to the
  search environment, this feature can be used to direct
  the search in subsequent calls to \code{searchNext}.
  VERBOSE=TRUE will print all intermediate search steps and
  results. It can be used to debug the expressions in the
  backtracker} \item{\code{$searchAll(...,
  VERBOSE=FALSE)}}{Return all solutions as a list}
  \item{\code{$reset()}}{Resets the \code{backtracker} to
  its initial state.} } }
}
\examples{

bt <- backtracker( isSolution= { 
                                 if (y == 0) return(TRUE)
                                 if (x == 0) return(FALSE)
                               }
                 , choiceLeft = { x <- x - 1; y <- y}
                 , choiceRight = { y <- y - 1; x <- x}
                 # starting values for x and y
                 , x=2
                 , y=1
                 )

bt$searchNext(VERBOSE=TRUE)
bt$searchNext(VERBOSE=TRUE)

# next search will return NULL because there is no more solution
bt$searchNext()

bt$reset()




}

