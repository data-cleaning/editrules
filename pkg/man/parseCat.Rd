\name{parseCat}
\alias{parseCat}
\title{Parse a categorical edit expression}
\usage{
  parseCat(x, val = NA, edit = logical(0), sep = ":",
    useLogical = FALSE, env = parent.frame())
}
\arguments{
  \item{x}{a valid R expression}

  \item{val}{logical (scalar)}

  \item{edit}{logical (vector)}

  \item{sep}{edit separator}

  \item{useLogical}{(logical), should logicals be treated
  as a factor or as a logical?}
}
\description{
  Parse a categorical edit expression
}
\keyword{internal}

