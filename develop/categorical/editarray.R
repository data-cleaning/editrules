#' Parse a categorical edit expression 
#'
#' @param x a valid R expression
#' @param val logical (scalar)
#' @param edit logical (vector)
#' @param sep edit separator
#' @nord
parseCond <- function(x, val=NA, edit=logical(0), sep=":"){
    if ( length(x) == 1 ) {
       edit[as.character(x)] <- val
       return(edit)
    }
    op <- as.character(x[[1]])
    if ( op == "if" ){
        edit <- parseCond(x[[2]],TRUE,  edit, sep)
        edit <- parseCond(x[[3]],FALSE, edit, sep)
    } else if ( op %in% c("(","{") ){
        edit <- parseCond(x[[2]], val,  edit, sep)
    } else if ( op %in% c("%in%","==") ){
        cat <- eval(x[[3]])
        if (is.logical(cat)){ 
          var <- as.character(x[[2]])
          if (!cat) val <- !val
        } else {
            var <- paste(x[[2]],cat,sep=sep)
        }
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
        stop("Operator '",op,"' not implemented")
    }
    edit
}

#' Parse textual, categorical edit rules to an editarray
#'
#' Transforms a list of categorical edit rules to a boolean array representation.
#' An editarry is used to store demands on purely categorical data.
#'
#' The purpose of this function is to turn human-readable demands on categorical data
#' to a boolean array. Categorical edit rules state demands on a dataset in the form of a
#' quoted R expression. Allowed statements include \code{if}, operators 
#' \code{%in%}, \code{==}, \code{!=}, \code{||}, \code{&&} and brackets \code{()} and \code{\{\}}.
#'
#' The datamodel is derived from the edit set. A data model can be defined by simply adding 
#' univariate edits of the form
#'
#' \itemize{
#' \item{\code{"<variable> %in% c('<cat1>','<cat2>',...,'<catN>')"}}
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
#' The result is an object of class \code<editarray>, which contains a \eqn{m\times n} boolean array, representing
#' the multivariate edits. The columns are labeled with \code{<variable><sep><category>}, for example \code{gender:Male}.
#' The column names represent the data model for the data  to be treated, the entries represent the edit rules. For example.
#' if the datamodel is \code{"gender %in% c('male','female')"} and \code{"pregnant %in% c('yes','no')"}, the edit
#' \code{"if(gender == 'male') pregnant == 'no'"} is represented by the boolean array 
#' \code{c(gender:male=TRUE, gender:female=FALSE, pregnant:yes=TRUE, pregnant:no=FALSE)}.
#'
#'
#'
#' @param editrules \code{character} vector 
#' @value editarray
#' @export
editarray <- function(editrules, sep=":"){
    e <- parseEdits(editrules)
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




