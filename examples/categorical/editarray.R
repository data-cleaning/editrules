parseEdits <- function(x){
    parse(text=x)
}

parseCond <- function(x, val=NA, edit=logical(0), sep=":"){
    if (length(x) == 1 ) x <- x[[1]]    
    op <- as.character(x[[1]])
    if ( op == "if" ){
        edit <- parseCond(x[[2]],TRUE,  edit, sep)
        edit <- parseCond(x[[3]],FALSE, edit, sep)
    }
    if ( op %in% c("%in%","==") ){
        var <- paste(x[[2]],eval(x[[3]]),sep=sep)
        edit[var] <- val
    } 
    edit
}


editarray <- function(x, sep=":"){
    e <- parseEdits(x)
    v <- lapply(e,parseCond,sep=sep)
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
        I <- apply(E[,J],1,function(e) all(is.na(e)) ) 
        E[I,J] <- TRUE
        # vars in edits
        E[,J] <-  t(apply(E[,J],1,function(e){
            val <- e[!is.na(e)][1]
            e[is.na(e)] <- !val
            e
        }))
    }

    neweditarray(E,ind)
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
neweditarray <- function(E, ind, names=NULL, levels=colnames(E)){
    if ( is.null(names) ) names <- paste("e",1:nrow(E),sep="")
    dimnames(E) <- list(edits=names,levels=levels)
    structure(E,
        class  = "editarray",
        ind    = ind
    )
}

# number of variables involved in the edits in E
nInvolved <- function(E){
    ind <- getInd(E)
    apply(E,1,function(e){
        sum(sapply(ind,
            function(I) if (sum(e[I]) < length(I))  1 else 0
        ))
    })
}


getVars.editarray <- function(E) names(attr(E,"ind"))

#' @nord
getInd <- function(E) attr(E,"ind")


#' @nord
getArr <- function(E) E[,,drop=FALSE]

#' @nord
getlevels <- function(E) colnames(E)
#' @nord
getnames <- function(E) rownames(E)


# determine which edits in an editmatrix contain a certain variable.
contains <- function(E,var){
    I <- getInd(E)[[var]]
    V <- getArr(E)[,I,drop=FALSE]
    rowSums(V) < length(getInd(E)[[var]])
}

