#' parse textual edits to R-expressions
#'
#' @param x character
#' @value R expression
parseEdits <- function(x){
    parse(text=x)
}

#' Parse a categorical edit expression 
#'
#' @param x a valid R expression
#' @param val logical (scalar)
#' @param edit logical (vector)
#' @param sep edit separator
#' @nord
parseCond <- function(x, val=NA, edit=logical(0), sep){
    if (length(x) == 1 ) x <- x[[1]] 
    op <- as.character(x[[1]])
    if ( op == "if" ){
        edit <- parseCond(x[[2]],TRUE,  edit, sep)
        edit <- parseCond(x[[3]],FALSE, edit, sep)
    } else if ( op %in% c("(","{") ){
        edit <- parseCond(x[[2]], val,  edit, sep)
    } else if ( op %in% c("%in%","==") ){
        var <- paste(x[[2]],eval(x[[3]]),sep=sep)
        edit[var] <- val
    } else if (op == "!=") {
        var <- paste(x[[2]],eval(x[[3]]),sep=sep)
        edit[var] <- !val
    } else if (op == "!") {
        edit <- parseCond(x[[2]],!val,  edit, sep)
    } else if (op == "&&"){
        if (val == FALSE){
            stop("Operator '&&' not allowed in 'if' clause")
        }
        edit <- parseCond(x[[2]],val, edit, sep)
        edit <- parseCond(x[[3]],val, edit, sep)
    } else if (op == "||"){
        if (val == TRUE){
            stop("Operator '||' not allowed in 'then' clause")
        }
        edit <- parseCond(x[[2]],val, edit, sep)
        edit <- parseCond(x[[3]],val, edit, sep)
    } else {
     #   stop("Operator '",op,"' not implemented")
    }
    edit
}

#' Parse textual edits to editarray
#'
#' @param x character vector 
#' @value editarray
#' @export
editarray <- function(x, sep=":"){
    e <- parseEdits(x)
    v <- lapply(e,parseCond,sep=sep)
    #return(v)
    
    # derive datamodel
    cols <- sort(unique(do.call(c,lapply(v,names))))
    
    # get variable names
    vr <- sub(paste(sep,".+","",sep=""),"",cols)
    vars <- unique(vr)
    
    # get categories
    cat <- sub(paste(".+",sep,sep=""),"",cols)
    
    # build indexing list
    ind <- lapply(vars, function(v) which(v==vr))
    ind <- lapply(ind,function(I) {names(I) <- cat[I];I})
    names(ind) <- vars
    
    # edits with NA only extend the data model.
    v <- v[!sapply(v,function(u) is.na(u[1]))]
        
    # set edtiarray values
    n <- length(cols)
    m <- length(v)
    E <- array(NA, dim=c(m,n), 
            dimnames = list(
                edits = paste("e",1:m,sep=""),
                variables = cols
            )
        )
    lapply(1:m,function(i) E[i,names(v[[i]])] <<- v[[i]])    
    for ( J in ind ){
        # vars not in any edit.
        I <- apply(E[,J,drop=FALSE],1,function(e) all(is.na(e)) ) 
        E[I,J] <- TRUE
        # vars in edits
        E[,J] <-  t(apply(E[,J,drop=FALSE],1,function(e){
            val <- e[!is.na(e)][1]
            e[is.na(e)] <- !val
            e
        }))
    }

    neweditarray(E,ind,sep=sep)
}


#' @nord
print.editarray <- function(x, ...){
    cat("editarray:\n")
    print(getArr(x))
}

#' editarray: logical array where every column corresponds to one
#' level of one variable. Every row is an edit. Every edit denotes
#' a *forbidden* combination.
#' @nord
neweditarray <- function(E, ind, sep, names=NULL, levels=colnames(E)){
    if ( is.null(names) & nrow(E)>0 ) names <- paste("e",1:nrow(E),sep="")
    dimnames(E) <- list(edits=names,levels=levels)
    structure(E,
        class  = "editarray",
        ind    = ind,
        sep    = sep
    )
}

#' number of variables involved in the edits in E
#'
#' Determines the number of variables involved in each edit in E.
#' A variable is involved in an edit if the boolean representation is not
#' TRUE for every category of that variable.
#'
#' @param E \code{\link{editmatrix}}
#' @value integer
#'
#' @nord
nInvolved <- function(E){
    ind <- getInd(E)
    apply(E,1,function(e){
        sum(sapply(ind,
            function(I) if (sum(e[I]) < length(I))  1 else 0
        ))
    })
}

#' get variable names in editarray
#' 
#' @param E \code{\link{editmatrix}
#' @value character vector
#' @nord
getVars.editarray <- function(E) names(attr(E,"ind"))

#' get index list from editmatrix
#' 
#' The 'ind' attribute is a named list of named integer vectors. The list names are the 
#' variable names. The vectors in the list index the columns in the editarray associated with the
#' variables. The names of the vectors are the names of the columns of the editarray.
#' 
#' @param E \code{\link{editarray}}
#' @value named list, indexing category levels in the editarray (columns)
#' @nord
getInd <- function(E) attr(E,"ind")


#' get seprator used to seperate variables from levels in editarray
#' @param E \code{\link{editarray}}
#' @value character
#' @nord
getSep <- function(E) attr(E,"sep")

#' Get named logical array from editarray
#' @param E \code{\link{editarray}}
#' @value logical array
#' @nord
getArr <- function(E) E[,,drop=FALSE]

#' retrieve level names from editarray
#' @param editarray \code{\link{editarray}}
#' @value character vector
#' @nord
getlevels <- function(E) colnames(E)

#' retrieve edit names from editarray
#' @param E \code{\link{editarray}}
#' @value character vector
#' @nord
getnames <- function(E) rownames(E)

#' determine which edits in an editmatrix contain a variable.
#'
#'
#' @param E \code{\link{editarray}}
#' @param var character, name of a categorical variable of \code{E}
#' @value \code{logical} vector of length nrow(E), TRUE for edits containing \code{var}
#' @export
contains <- function(E,var){
    I <- getInd(E)[[var]]
    V <- getArr(E)[,I,drop=FALSE]
    rowSums(V) < length(getInd(E)[[var]])
}

#' Summarize data model of an editarray in a data.frame
#' @export
#' @param E editarray
#' @value data.frame describing the categorical variables and their levels.
#' 
datamodel <- function(E){
    st <- stack(getInd(E))
    data.frame(variable=as.character(st[,2]),value=rownames(st))
}




