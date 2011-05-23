
# simple edit manipulation with categorical edits.
# m, 04.05.2011

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

#
# column indices in editarray, for every variable
editindex <- function(dm){
    S <- as.character(stack(dm)[,2])
    sapply(names(dm), function(n) which(n==S)) 
}



#' @nord
print.editarray <- function(x, ...){
    cat("editarray:\n")
    print(unclass(x)[,])
}

#' editarray: logical array where every column corresponds to one
#' level of one variable. Every row is an edit. Every edit denotes
#' a *forbidden* combination.
#' @nord
neweditarray <- function(E, ind, names=rownames(E), levels=colnames(E)){
    if ( is.null(names) ) names <- paste("e",1:nrow(E),sep="")
    dimnames(E) <- list(edits=names,levels=levels)
    structure(E,
        class  ="editarray",
        ind    = ind
    )
}

#' Generate a single edit restriction
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
    e <- !array(logical(d),dim=c(1,d))
    colnames(e) <- c(dm,recursive=TRUE)
    rownames(e) <- name
    I <- editindex(dm) 
    for ( x in names(L) ){
        e[ 1, I[[x]][ !(dm[[x]] %in% L[[x]]) ] ] <- FALSE
    }

    neweditarray(e, ind = I, names=name, levels=c(dm,recursive=TRUE))
}


#' combine edit restrictions to an array
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
getArr <- function(E) E[,,drop=FALSE]


#' @nord
getlevels <- function(E) colnames(E)
#' @nord
getnames <- function(E) rownames(E)


# combine editarray by generating variable "var"
# intersection <-> and, union <-> or
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

# determine which edits in an editmatrix contain a certain variable.
contains <- function(E,var){
    I <- getInd(E)[[var]]
    V <- getArr(E)[,I,drop=FALSE]
    rowSums(V) < length(getInd(E)[[var]])
}


# eliminate one category from a logical array
eliminateCat <- function(A, J, j){
    j1 <- A[,J[j]]
    j2 <- !j1
    n1 <- sum(j1)
    n2 <- sum(j2)
    if (n1==0 || n2==0) return(A)
    I1 <- rep(which(j1), times=n2)
    I2 <- rep(which(j2), each=n1)
    B <- array(FALSE,dim=c(n1*n2,ncol(A)))
    B[,J] <- A[I1,J,drop=FALSE] | A[I2,J,drop=FALSE]
    B[,-J] <- A[I1,-J,drop=FALSE] & A[I2,-J,drop=FALSE]
    B
}

# TODO  1. prove that this works --DONE 23.05.2011
#       2. redundancy removal    --DONE. 23.05.2011
#       3. robustness for empty arrays etc. --Needs testing
eliminateVar <- function(E, var){
    J <- getInd(E)[[var]]
    A <- getArr(E)
    for ( j in 1:length(J)){
         red <- duplicated(A) | isObviouslyRedundant.array(A)
         A <- eliminateCat(A[!red,],J,j)
    }
    neweditarray(A,getInd(E), levels=getlevels(E))
}

# duplicated method for editarray
duplicated.editarray <- function(x, ...) duplicated(getArr(E))

# redundancy check - editarray method.
isObviouslyRedundant.editarray <- function(E, ...){
    isObviouslyRedundant.array(getArr(E))
}

# redundancy check (check if any edit rule is a subset of another one)
isObviouslyRedundant.array <- function(E, ...){
    # TODO: is this any faster with a while loop? Should we do this in C?
    m <- nrow(E)
    m1 <- m-1
    sapply(1:m, function(i){
        any(rowSums(E[-i,,drop=FALSE] - (E[rep(i,m1),,drop=FALSE] | E[-i,,drop=FALSE])) == 0)
    })
}


# replace a value in an editmatrix: 
#   remove rows of E for which have var[value] == FALSE
#   set levels of var[!value] to FALSE
replaceValue.editmatrix <- function(E, var, value){
    J <- getInd(E)[[var]]
    ival <- intersect(which(colnames(E) == value), J)
    if ( length(ival) != 1 ) 
        stop(paste("Variable ", var,"not present in editarray or cannot take value",value))
    ii <- setdiff(J,ival)
    A <- getArr(E)
    A[,ii] <- FALSE
    I <- A[,ival] 
    neweditarray(A[I,,drop=FALSE], getInd(E), levels=getlevels(E))
}


# example from method series
D <- data.model(
    civilStatus = c("married","unmarried","widowed","divorced"),
    age = c("< 16",">= 16"),
    positionInHousehold = c("marriage partner","child","other")
)

# edits:
#   1. you cannot be married under 16 yrs
#   2. you cannot be a marriage partner in the household if you're not married
E <- editarray(
    forbid(D, age = "< 16", civilStatus = "married"),
    forbid(D, civilStatus = c("unmarried", "widowed","divorced"), 
        positionInHousehold = "marriage partner"),
    forbid(D, civilStatus = c("widowed","divorced"), 
        positionInHousehold = "marriage partner")
)

# derived edit, by eliminating civilStatus: you cannot be a marriage
# partner in a household whe you're under 16:
eliminateVar(E,"civilStatus")



#D <- data.model(
#    sex = c("male","female"),
#    pregnant = c("yes","no"),
#    age = c("child","adult","old age")
#)

# define some edits
#E <- editarray(
#    forbid(D, sex="male", pregnant="yes", name="e1"),
#    forbid(D, age=c("child", "old age"), pregnant="yes", name="e2")
#)
# derive a new edit, using "age" as generating variable.
#combine(E,"age")

# should yield the same as this:
# forbid(D, pregnant="yes", age=c("child","old age"))





