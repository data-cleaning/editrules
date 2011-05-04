


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

# retrieve list of column indices for edit array.
editindex <- function(dm){
    S <- as.character(stack(D)[,2])
    sapply(names(D), function(n) which(n==S)) 
}



#' @nord
print.editarray <- function(x, ...){
    cat("editarray:\n")
    print(unclass(x)[,])
}


#' @nord
neweditarray <- function(E, ind, names=rownames(E), levels=colnames(E)){
    if ( is.null(names) ) names <- paste("e",1:nrow(E),sep="")
    dimnames(E) <- list(edits=names,levels=levels)
    structure(E,
        class  ="editarray",
        ind    = ind
    )
}

#' 
#'
#'
#' @param ... edit restrictions in the form <var>=c("val1", "val2")
#' @param dm \code{\link{data.model}}
#' @param name name of edit.
#'
#' @return one-row \code{\link{editarray}} 
#' @nord
forbid <- function(dm, ..., name=NULL){

    L <- list(...)
    if ( !all(names(L) %in% names(dm)) )
        stop("edit contains variables not in data.model")
    
    d <- sum(dim(dm))
    e <- array(logical(d),dim=c(1,d))
    colnames(e) <- c(dm,recursive=TRUE)
    rownames(e) <- name
    I <- editindex(dm) 
    for ( x in names(L) ){
        e[ 1, I[[x]][ D[[x]] %in% L[[x]] ] ] <- TRUE
    }

    neweditarray(e, ind = I, names=name, levels=c(dm,recursive=TRUE))
}


#' combine 1-d edit rows to an edit array
#'
#' @param ... objects of class \code{\link{editarray}}
#'
#' @nord
editarray <- function(...){
    neweditarray(rbind(...), ind = attr(..1,"ind") )
}

#' @nord
getInd <- function(E) attr(E,"ind")

#' @nord
getArr <- function(E) E[,]


#' @nord
getlevels <- function(E) colnames(E)
#' @nord
getnames <- function(E) rownames(E)


# verboden: doorsnede <-> or, vereniging <-> and
combine <- function(E,var){ 
    ind <- getInd(E)
    lev <- getlevels(E)
    Arr <- getArr(E)
    I <- ind[[var]]
    n <- ncol(Arr)
    J <- 1:n
    J <- c(J[I],J[-I])
    e <- array(logical(n),dim=c(1,n))
    e[,J] <-  c(apply(Arr[, I],2,any), apply(Arr[, -I],2,all))
    neweditarray(e,ind, levels=lev)
}


# define a data.model
D <- data.model(
    sex = c("male","female"),
    pregnant = c("yes","no"),
    age = c("child","adult","old age")
)

# define some edits
E <- editarray(
    forbid(D, sex="male", pregnant="yes", name="e1"),
    forbid(D, age=c("child", "old age"), pregnant="yes", name="e2")
)
# derive a new edit, using "age" as generating variable.
combine(E,"age")







