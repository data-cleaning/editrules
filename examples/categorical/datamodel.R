# Define levels of categorical variables
# returns S3 class data.model (ragged array of character vecors)
#' @nord
data.model <- function(...){
    L <- list(...)
    levelnames <- 
    if ( !all(sapply(L,is.character)))
        stop("Arguments must be character vecors")
    d <- sapply(L,length)
    structure(L,
        class="data.model",
        dims = d
    )
}

#' @nord
dim.data.model <- function(x){
    attr(x,"dim")
}


#' @nord
str.data.model <- function(object,...){
    x <- sapply(object,function(d){
        v <- paste(d,collapse=", ")
        if (nchar(v) > 80 ){
            v <- paste(strtrim(v,77),"...")
        }
        v
    })
    cat("data.model:\n")
    cat( paste(" ",names(object)," (",dim(object),") : ",x,sep="",collapse="\n") )
    cat("\n")
}

#' @nord
print.data.model <- function(x,...){
    str(x)
}

