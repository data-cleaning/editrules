
#' print  editarray
#'
#' @method print editarray
#' @param x an \code{\link{editarray}}
#' @param ... arguments to be passed to or from other methods.
#' @keywords internal
#' @export
print.editarray <- function(x, ...){
    d <- datamodel(x)
    cn <- paste(abbreviate(d$variable),":",abbreviate(d$value),sep="")
    A <- getArr(x)
    colnames(A) <- cn
    cat("Edit array:\n")
    print(A)
    cat("\nEdit rules:\n")
    d <- as.data.frame(x)
    cat(paste(d$name," : ",d$edit,collapse="\n"),"\n")
}



#' print edit matrix
#'
#' @export
#' @method print editmatrix
#'
#' @param x editmatrix object to be printed
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
print.editmatrix <- function(x, ...){
   cat("Edit matrix:\n")
   print(as.data.frame(x), ...)
   cat("\nEdit rules:\n")
   info <- editrules(x)
   desc <- paste("[",info$description,"]")
   desc <- ifelse(info$description=="","", desc)
   cat( paste( info$name,":", info$edit, desc, collapse="\n")
      , "\n"
      )
}


