#' Parse textual, categorical edit rules to an editarray
#'
#' Transforms a list of categorical edit rules to a boolean array representation.
#' An editarry is used to store demands on purely categorical data.
#'
#' The purpose of this function is to turn human-readable demands on categorical data
#' to a boolean array. Categorical edit rules state demands on a dataset in the form of a
#' quoted R expression. Allowed statements include \code{if}, operators 
#' \code{\%in\%}, \code{==}, \code{!=}, \code{||}, \code{&&} and brackets \code{()} and \code{\{\}}.
#'
#' The datamodel is derived from the edit set. A data model can be defined by simply adding 
#' univariate edits of the form
#'
#' \itemize{
#' \item{\code{"<variable> \%in\% c('<cat1>','<cat2>',...,'<catN>')"}}
#' }
#'
#' Note the double quotes around the whole statement, and the single quotes around the category levels.
#' The right hand side is evaluated when the editarray is generated, so it may also be the name of a
#' previously defined variable. Also see the examples section.
#'
#' Relations between variables can be written in the form
#'
#' \itemize{
#' \item{\code{"if( <logical expression involving categorical variables> ) <logical expression involving categorical variables>"}}
#' }
#' See the example section for some coded examples.
#'
#' The result is an object of class \code{editarray}, which contains a \eqn{m\times n} boolean array, representing
#' the multivariate edits. The columns are labeled with \code{<variable><sep><category>}, for example \code{gender:Male}.
#' The column names represent the data model for the data  to be treated, the entries represent the edit rules. For example.
#' if the datamodel is \code{"gender \%in\% c('male','female')"} and \code{"pregnant \%in\% c(TRUE,FALSE)"}, the edit
#' \code{"if(gender == 'male') pregnant == FALSE"} is represented by the boolean array 
#' \code{c(gender:male=TRUE, gender:female=FALSE, pregnant:TRUE=TRUE, pregnant:FALSE=FALSE)}.
#'
#'
#'
#' @param editrules \code{character} vector 
#' @param sep textual separator, to be used internally for separating variable from category names. 
#' @return editarray
#'
#' @example ../examples/editarray.R
#'
#'
#' @export
editarray <- function(editrules, sep=":"){
    e <- parseEdits(editrules)
    v <- lapply(e,parseCat,sep=sep)
    
    # find always FALSE edits:
    iNull <- sapply(v,is.null)  
    
    # derive datamodel
    cols <- sort(unique(do.call(c,lapply(v[!iNull],names))))
    # get variable names
    vr <- sub(paste(sep,".+","",sep=""),"",cols)
    vars <- unique(vr)
    
    # get categories
    cat <- sub(paste(".+",sep,sep=""),"",cols)
    
    # build indexing list
    ind <- lapply(vars, function(v) which(v==vr))
    ind <- lapply(ind,function(I) {names(I) <- cat[I];I})
    names(ind) <- vars
    
    # edits with NA only extend the data model, is.null detects the always FALSE edit
    v <- v[sapply(v,function(u) is.null(u) || !is.na(u[1]))]
    # replace NULL with NA so the allways FALSE edit is included explicitly
    if ( length(v) > 0 )  v[sapply(v,is.null)] <- NA
        
    # set editarray values
    n <- length(cols)
    m <- length(v)
    if ( m == 0 ){
        return(neweditarray(
            array(logical(0),dim=c(m,n),dimnames=list(edits=NULL,variables=cols)),ind,sep)
        )

    }
    E <- array(NA, dim=c(m,n), 
            dimnames = list(
                edits = paste("e",1:m,sep=""),
                variables = cols
            )
        )
    lapply(1:m,function(i) E[i,names(v[[i]])] <<- v[[i]])    
    # per variable, the boolean values not filled in during parsing must be derived.
    # they are the opposite from allready filled in values, or in case they are not involved,
    # all TRUE.
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

#' Derive textual representation from (partial) indices
#'
#' 
#'
#' @param ind a \code{list}, usually a (part of) the 'ind' attribute of an editarray
#' @param invert \code{logical} vector of  lenght length(ind)
#'
#' @return For every entry in \code{ind}, a character vector is returned where each entry
#'      is a statement of the form \code{"<var> \%in\% c('<cat1>',...,'<catN>')" or "<var> == '<catt>'"}
#'      if invert==TRUE. If invert==FALSE, the negation of the above statements is returned.
#'
#' @seealso as.character.editarray
#'
#' not for export
#' @keywords internal
ind2char <- function(ivd, ind=ivd, invert=logical(length(ivd)),useEqual=TRUE){
    v <- names(ivd)
    cats <- lapply(ivd, function(k) paste("'", names(k), "'", sep=""))
    op <- rep("%in%",length(ivd))
    l <- sapply(cats,length)
    if (useEqual){
        op[l == 1 & !invert] <- "=="
        op[l == 1 &  invert] <- "!="
    }
    cats[l>1] <- lapply(cats[l>1], function(cc) paste("c(",paste(cc,collapse=", "),")",sep=""))
    u <- paste(v,op,cats)
    u[l>1 &  invert] <- paste("!(",u[l>1 & invert],")")
    u <- sub("'FALSE'","FALSE",u)
    u <- sub("'TRUE'","TRUE",u)
    u <- sub("!= FALSE","== TRUE",u)
    u <- sub("!= TRUE","== FALSE",u)
    u
}


#' Coerce an editarray to \code{character}
#'
#' Coerces an editarray to a \code{data.frame}. The resulting character vector can be reparsed 
#' to an editarray with \code{\link{editarray}}. The datamodel (the set of categories for every variable)
#' is represented in the 'ind' attribute of an editarray. The character representation will contain
#' a number of entries, named \code{d}\eqn{i}, of the form \code{"<variable> \%in\% c('<cat1>',...,'<catN>')"}. 
#'
#' @method as.character editarray
#' @param x editarray object
#' @param useIf \code{logical}. Use if( <condition> ) <statement> or !<condition> | <statement> ? 
#' @param datamodel \code{logical}. Include datamodel explicitly?
#' @param ... further arguments passed to or from other methods
#'
#' @export
as.character.editarray <- function(x, useIf=TRUE, datamodel=TRUE, ...){
    A <- getArr(x)
    if (ncol(A) == 0 ){ 
        s <- character(nrow(A))
        if ( nrow(A)>0 ) names(s) <- rownames(A)
        return(s)
    }
    ind <- getInd(x)
    dm <- c()
    if ( datamodel ){
        dm <- ind2char(ind,useEqual=FALSE)
        names(dm) <- paste("d",1:length(dm),sep="")
    }
    # edits
    if ( nrow(A) == 0 ) return(dm)
    edts <- character(nrow(A))
    for ( i in 1:nrow(A) ){
        a <- A[i,]
        involved <- sapply(ind, function(J) sum(a[J]) < length(J))
        # corner case: every record fails the edit (all categories TRUE).
        #if (!any(involved)) involved <- !involved
        
        ivd <- ind[involved]
        ivd <- lapply(ivd, function(J) J[a[J]])
        if ( length(ivd) == 0 ){
            edts[i] <- FALSE
        } else if ( length(ivd) == 1 ){
            edts[i] <- ind2char(ivd, ind)
        } else {
            
            n <- length(ivd)
            inv <- logical(n)
            inv[n] <- TRUE
            # corner case: all categories TRUE
            #if (all(involved) ) inv <- !inv
            ch <- ind2char(ivd, ind, invert=inv)
            if ( useIf ){
                edts[i] <- paste("if(", paste(ch[1:(n-1)],collapse=" & "), ")",ch[n])
            } else {
                edts[i] <- paste("!(", paste(ch[1:(n-1)],collapse=" & "), ") |",ch[n])
            }
        }
    }
    names(edts) <- rownames(x)
    # add datamodel and return
    c(dm, edts) 
}


#' Coerce an editarray to a \code{data.frame}
#'
#' Coerces an editarray to a \code{data.frame}. 
#'
#' @method as.data.frame editarray
#' @param x editmatrix object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{as.character.editarray}}
#' @return data.frame with columns 'name', 'edit' and 'description'.
#'
#' @export 
as.data.frame.editarray <- function(x, ...){
    edts <- as.character(x, ...)
    data.frame(name=names(edts),edit=edts,description=character(length(edts)),row.names=NULL)
}

#' Coerce an editarray to R expressions
#'
#' Generates an R \code{expression} vector that can be used to check data using \code{\link{eval}}.
#' @export
#' @method as.expression editarray
#'
#' @param x editarray object to be parsed
#' @param ... further arguments passed to or from other methods.
as.expression.editarray <- function(x, ...){
  return(
    tryCatch(parse(text=as.character(x, ...)), 
             error=function(e){
               stop(paste("Not all edits can be parsed, parser returned", e$message,sep="\n"))
             }
             )
    )
}


#' editarray: logical array where every column corresponds to one
#' level of one variable. Every row is an edit. Every edit denotes
#' a *forbidden* combination.
#' @keywords internal
neweditarray <- function(E, ind, sep, names=NULL, levels=colnames(E),...){
    if ( is.null(names) & nrow(E)>0 ) names <- paste("e",1:nrow(E),sep="")
    dimnames(E) <- list(edits=names,levels=levels)
    structure(E,
        class  = "editarray",
        ind    = ind,
        sep    = sep,
        ...
    )
}


