#' Facilitates reading and manipulation of linear
#' (in)equalities, converting them into matrix format (and vice versa)
#' and checking if data are valid according to these rules. Sets of
#' linear edits can be manipulated using Fourier-Motzkin elimination
#' and variable substitution and solving error localization problems
#' based on the generalized principle of Felligi and Holt.
#' 
#' The \code{editrules} package aims to provide an environment to conveniently define,
#' read and check linear (in)equality restrictions, perform common of rule sets
#' manipulations and offer error localization functionality based on the
#' (generalized) paradigm of Fellige Holt. This paradigm assumes
#' that errors are distributed randomly over the variables and there
#' is no detectable cause of error. It also decouples the detection of corrupt variables from their correction. 
#' For some types of error, such as sign flips,
#' typing errors or rounding errors, this assumption does not hold. These errors can be detected and are closely 
#' related to their resolution. The reader is referred to the package \code{deducorrect}
#' package for treating such errors. 
#'
#' Typical usage is:
#' @example examples/editrules.R
#' @name editrules-package 
#' @docType package 
{}
