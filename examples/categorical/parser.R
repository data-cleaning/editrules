
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
    } else {
        stop("Operator not implemented")
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

    list(E,ind)
}

edts <- c(
    "geslacht %in% c('man','vrouw')",
    "zwanger %in% c('JA','NEE')",
    "if (geslacht %in% c('man')) zwanger == 'NEE' ",
    "if (leeftijd %in% c('<16','>=60')) zwanger == 'NEE'")

L<-editarray(edts)
L[[1]]







