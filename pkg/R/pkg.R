#' Facilitates reading of lineair (in)equalities and
#'    converting them to a matrix format (and vice versa)
#' 
#' Edit rules facilitates the reading/parsing of so called "edit rules" or constraints. These are linear (in)equalities used for checking
#' statistical data. If an observation (row) passes these rules it is considered valid or plausible. 
#' If an observation doesn't pass the rules then it should be checked and eventually edited.
#' Important for the (automatic) editing proces are the rules that are violated. 
#' 
#' The function \code{\link{editmatrix}} reads lineair (in)equalities.
#' Typical usage is:
#' @example examples/editrules.R
#' @name editrules-package 
#' @docType package 
NULL