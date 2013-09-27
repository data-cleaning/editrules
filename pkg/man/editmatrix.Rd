\name{as.matrix.editmatrix}
\alias{as.character.editmatrix}
\alias{as.data.frame.editmatrix}
\alias{as.expression.editmatrix}
\alias{as.matrix.editmatrix}
\alias{c.editmatrix}
\alias{editmatrix}
\alias{str.editmatrix}
\alias{summary.editmatrix}
\title{Create an editmatrix}
\usage{
  \method{as.matrix}{editmatrix} (x, ...)

  \method{c}{editmatrix} (...)

  editmatrix(editrules, normalize = TRUE)

  \method{as.data.frame}{editmatrix} (x, ...)

  \method{as.character}{editmatrix} (x, ...)

  \method{as.expression}{editmatrix} (x, ...)

  \method{str}{editmatrix} (object, ...)

  \method{summary}{editmatrix} (object, useBlocks = TRUE,
    ...)
}
\arguments{
  \item{editrules}{A \code{character} or \code{expression}
  vecotr with (in)equalities written in R syntax.
  Alternatively, a \code{data.frame} with a column named
  \code{edits}, see details.}

  \item{normalize}{\code{logical} specifying if all edits
  should be transformed (see description)}

  \item{x}{editmatrix object}

  \item{object}{an R object}

  \item{useBlocks}{\code{logical} Summarize each block?}

  \item{...}{Arguments to pass to or from other methods}
}
\value{
  \code{as.matrix}: Augmented \code{matrix} of
  \code{editmatrix}. (See also \code{\link{getAb}}).

  \code{editmatrix} : An object of class \code{editmatrix}

  \code{as.data.frame} a 3-column \code{data.frame} with
  columns 'name' and 'edit'. If the input editmatrix has a
  \code{description} attribute a third column is returned.
}
\description{
  An \code{editmatrix} is a numerical matrix and a set of
  comparison operators representing a linear system of
  (in)equations.
}
\details{
  The function \code{editmatrix} generates an editmatrix
  from a \code{character} vector, an \code{expression}
  vector or a \code{data.frame} with at least the column
  \code{edit}. The function \code{\link{editfile}} reads
  edits from a free-form textfile, function
  \code{\link{as.editmatrix}} converts a matrix, a vector
  of constants and a vector of operators to an
  \code{editmatrix}

  By default, the \code{editmatrix} is normalized, meaning
  that all comparison operators are converted to one of
  \code{<}, \code{<=}, or \code{==}. Users may specify
  edits using any of the operators \code{<, <=, ==, >=, >}
  (see examples below). However it is highly recommended to
  let \code{editmatrix} parse them into normal form as all
  functions operating on editmatrices expect or convert it
  to normal form anyway.
}
\note{
  since version 2.0-0, the behaviour of
  \code{as.data.frame.editmatrix} changed to be more
  symmetrical with \code{editmatrix.data.frame} and
  \code{as.data.frame.editarray}. Use
  \code{editrules:::toDataFrame} (unsupported) for the old
  behaviour.
}
\examples{
# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# Using a expression vector to define contraints
E <- editmatrix(expression(x+3*y==2*z, x==z))
print(E)

# an editmatrix also has a summary method:
summary(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)
print(E.df)

E <- editmatrix(E.df)
print(E)
# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# Using a expression vector to define contraints
E <- editmatrix(expression(x+3*y==2*z, x==z))
print(E)

# an editmatrix also has a summary method:
summary(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)
print(E.df)

E <- editmatrix(E.df)
print(E)
}
\seealso{
  \code{\link{editrules.plotting}},
  \code{\link{violatedEdits}},
  \code{\link{localizeErrors}}, \code{\link{normalize}},
  \code{\link{contains}}, \code{\link{is.editmatrix}},
  \code{\link{getA}}, \code{\link{getAb}},
  \code{\link{getb}}, \code{\link{getOps}}
  \code{\link{getVars}}, \code{\link{eliminate}},
  \code{\link{substValue}}, \code{\link{isFeasible}}
}

