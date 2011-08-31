
#' get index list from editmatrix
#' 
#' The 'ind' attribute is a named list of named integer vectors. The list names are the 
#' variable names. The vectors in the list index the columns in the editarray associated with the
#' variables. The names of the vectors are the names of the columns of the editarray.
#' 
#' @param E \code{\link{editarray}}
#' @return named list, indexing category levels in the editarray (columns)
#' @keywords internal
getInd <- function(E) attr(E,"ind")


#' get seprator used to seperate variables from levels in editarray
#' @param E \code{\link{editarray}}
#' @return character
#' @keywords internal
getSep <- function(E) attr(E,"sep")

#' Get named logical array from editarray
#' @param E \code{\link{editarray}}
#' @return logical array
#' @keywords internal
getArr <- function(E) E[,,drop=FALSE]

#' retrieve level names from editarray
#' @param editarray \code{\link{editarray}}
#' @return character vector
#' @keywords internal
getlevels <- function(E) colnames(E)

#' retrieve edit names from editarray
#' @param E \code{\link{editarray}}
#' @return character vector
#' @keywords internal
getnames <- function(E) rownames(E)

#' determine which edits in an editmatrix contain a variable.
#'
#'
#' @param E \code{\link{editarray}}
#' @param var character, name of a categorical variable of \code{E}
#' @return \code{logical} vector of length nrow(E), TRUE for edits containing \code{var}
#' @export
contains <- function(E,var){
    if ( !is.editarray(E) ) stop("Argument not of class editarray")
    I <- getInd(E)[[var]]
    V <- getArr(E)[,I,drop=FALSE]
    rowSums(V) < length(getInd(E)[[var]])
}

#' Summarize data model of an editarray in a data.frame
#'
#' @param E editarray
#' @return \code{data.frame} describing the categorical variables and their levels.
#' 
#' @export
datamodel <- function(E){
    st <- stack(getInd(E))
    data.frame(variable=as.character(st[,2]),value=rownames(st))
}

