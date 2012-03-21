# TODO: give better example

#' Package editrules
#'
#' 
#' The \code{editrules} package aims to provide an environment to conveniently
#' define, read and check recordwise data restriction including 
#' \itemize{
#' \item{Linear (in)equality restrictions},
#' \item{Restrictions on categorical value combinations}
#' \item{Condtional restrictions on numerical or mixed data}
#' }
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
#'
#' @name editrules-package 
#' @docType package 
{}
