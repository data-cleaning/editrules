\name{newerrorlocation}
\alias{newerrorlocation}
\title{Generate new errorlocation object}
\usage{
  newerrorlocation(adapt, status, call = sys.call(-1),
    method, user = Sys.info()["user"], timestamp = date())
}
\arguments{
  \item{adapt}{Logical array of same dimensions of dataset,
  indicating which variable have to be changed per record}

  \item{status}{data.frame}

  \item{call}{A \code{call} object}

  \item{user}{\code{character} username}

  \item{timestamp}{\code{character} returned by
  \code{date()}.}
}
\description{
  Generate new errorlocation object
}
\keyword{internal}

