
#' print  editarray
#'
#' @method print editarray
#' @param x an \code{\link{editarray}}
#' @param ... arguments to be passed to or from other methods.
#' @keywords internal
#' @export
print.editarray <- function(x, ...){
    d <- datamodel(x)
    A <- getArr(x)
    if ( ncol(A) > 0 ){
        cn <- paste(abbreviate(d$variable),":",abbreviate(d$value),sep="")
        colnames(A) <- cn
    }
    cat("Edit array:\n")
    print(A)
    cat("\nEdit rules:\n")
    desc <- attr(x,'description')
    if ( is.null(desc) ){ 
        desc <- rep("",nrow(x))
    } else {
        desc <- paste('[',desc,']')
    }
    u <- as.character(x)
    nm <- names(u)
    pr <- paste(format(nm,width=max(nchar(nm))),':',paste(u,desc),collapse='\n')
    cat(pr,'\n')
}



#' print editmatrix
#'
#' @export
#' @method print editmatrix
#'
#' @param x editmatrix object to be printed
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
print.editmatrix <- function(x, ...){
    cat("Edit matrix:\n")
    print(toDataFrame(x), ...)
    cat("\nEdit rules:\n")
    desc <- attr(x,'description')
    if ( is.null(desc) ){ 
        desc <- rep("",nrow(x))
    } else {
        desc <- paste('[',desc,']')
    }
    u <- as.character(x)
    nm <- names(u)
    pr <- paste(format(nm,width=max(nchar(nm))),':',paste(u,desc),collapse='\n')
    cat(pr,'\n')
}

#' print cateditmatrix
#'
#' @export
#' @method print cateditmatrix
#'
#' @param x cateditmatrix object to be printed
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
print.cateditmatrix <- function(x, ...){
    cat("Edit matrix:\n")
    print(as.data.frame(x), ...)
    cat("\nEdit rules:\n")
    desc <- attr(x,'description')
    if ( is.null(desc) ){ 
        desc <- rep("",nrow(x))
    } else {
        desc <- paste('[',desc,']')
    }
    u <- as.character(x)
    u <- sub('<=','=>',u)
    nm <- names(u)
    pr <- paste(format(nm,width=max(nchar(nm))),':',paste(u,desc),collapse='\n')
    cat(pr,'\n')
}

#' print editset
#'
#' @export
#' @method print editset
#'
#' @param x editset object to be printed
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
print.editset <- function(x, ...){
    u <- as.character(x,datamodel=FALSE)
    v <- as.character(x,datamodel=TRUE)
    cnd <- attr(x,'condition')
    if (!is.null(cnd)){
        cat('Conditions:\n')
        cat(paste(cnd,collapse=', '),'\n\n')
    }
    cat("Data model:\n")
    v <- v[! v%in% u]
    if ( length(v)>0 ){
        nm <- names(v)
        cat(paste(format(nm,width=max(nchar(nm))),':',v,collapse='\n'),'\n')
    }
    cat("\nEdit set:\n")
    nm <- names(u)
    cat(paste(format(nm,width=max(nchar(nm))),':',u,collapse='\n'),'\n')
}


#' print editset
#'
#' 
#' @method print dnf
#'
#' @param x editset object to be printed
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
print.dnf <- function(x, ...){
    cat("Editset in Disjunctive Normal Form:\n\n")
    lapply(x,function(i) {cat("\n");print(i)}, ...)
}

