#' An overview of the function of package \code{editrules}
#'
#' 
#' The \code{editrules} package aims to provide an environment to conveniently
#' define, read and check recordwise data constraints including 
#' \itemize{
#' \item{Linear (in)equality constraints for numerical data},
#' \item{Constraints on value combinations of categorical data}
#' \item{Conditional constraints on numerical and/or mixed data}
#' }
#' In literature these constraints, or restrictions are refered to as ``edits''. 
#' \code{editrules} can perform common rule
#' set manipulations like variable elimination and value substitution, and 
#' offers error localization functionality based on the
#' (generalized) paradigm of Fellegi and Holt. Under this paradigm, one determines
#' the smallest (weighted) number of variables to adapt such that no (additional or derived) 
#' rules are violated. The paradigm is based on the assumption that errors
#' are distributed randomly over the variables and there is no detectable cause of
#' error. It also decouples the detection of corrupt variables from their
#' correction. For some types of error, such as sign flips, typing errors or
#' rounding errors, this assumption does not hold. These errors can be detected
#' and are closely related to their resolution. The reader is referred to the
#' \code{deducorrect} package for treating such errors. 
#'
#' @section I. Define edits:
#'
#' \code{editrules} provides several methods for creating edits from a \code{character}
#' , \code{expression}, \code{data.frame} or a text file.
#' \tabular{ll}{
#'   \code{\link{editmatrix}} \tab Create a linear constraint matrix for numerical data \cr
#'   \code{\link{editarray}}  \tab Create value combination constraints for categorical data \cr
#'   \code{\link{editset}}     \tab Create conditional numerical, numerical and categorical constraints \cr
#'   \code{\link{editfile}}     \tab Read  conditional numerical, numerical and categorical constraints
#' from a text file\cr
#' }
#'
#' @section II. Check and find errors in data:
#'
#' \code{editrules} provides several method for checking \code{data.frame}s with edits
#' \tabular{ll}{
#'   \code{\link{checkRows}} \tab TODO \cr
#'   \code{\link{violatedEdits}} \tab TODO \cr
#'   \code{\link{localizeErrors}}  \tab TODO \cr
#'   \code{\link{errorLocalizer}}  \tab TODO \cr
#' }
#' Note that you can call \code{plot}, \code{summary} and \code{print}  on results of these functions.
#'
#' @section IV. Manipulate and check edits:
#'
#' \code{editrules} provides several methods for manipulating edits
#' \tabular{ll}{
#'   \code{\link{substValue}} \tab TODO \cr
#'   \code{\link{eliminate}} \tab TODO \cr
#'   \code{\link{isFeasible}} \tab TODO \cr
#'   \code{\link{duplicated}} \tab TODO \cr
#'   \code{\link{blocks}} \tab TODO \cr
#'   \code{\link{reduce}} \tab TODO \cr
#'   \code{\link{simplify}} \tab TODO \cr
#'   \code{\link{seperate}} \tab TODO \cr
#'   \code{\link{generateEdits}} \tab TODO \cr
#' }
#'
#' @section V. Plot and coerce edits:
#'
#' \code{editrules} provides several methods for manipulating edits
#' \tabular{ll}{
#'   \code{\link{plot.editset}} \tab TODO \cr
#'   \code{\link{as.igraph}} \tab TODO \cr
#'   \code{\link{as.character}} \tab TODO \cr
#'   \code{\link{as.data.frame}} \tab TODO \cr
#' }
#'
#' @name editrules-package 
#' @docType package 
{}
