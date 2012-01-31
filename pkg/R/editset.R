#' Create an editset which can contain a mix of categorical, numerical and mixededits
#' 
#' NOTE: at the moment, functionality for mixed edit sets is limited and somewhat experimental.
#'
#' @param editrules \code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
#'        a \code{character} or \code{expression} with (in)equalities written in R syntax
#' @param env environment to parse categorical edits in
#' @export
editset <- function(editrules, env=new.env()){
    
    # detect edit types
    num <- parseEdits(editrules, type="num")
    cat <- parseEdits(editrules, type="cat")
    mix <- parseEdits(editrules, type="mix")
  

    # pure numerical edits    
    num <- editmatrix(num)
    nnum <- nrow(num)
    # pure categorical edits    
    cat <- editarray(cat)
    ncat <- nrow(cat)
    if ( ncat > 0 ) rownames(cat) <- paste("cat",(nnum+1):(nnum+ncat),sep="")


    nmix <- length(mix)
    mixnum <- expression()
    mixcat <- vector(mode="expression", nmix)
    if ( nmix > 0 ){ 
        mixl <- vector(mode="list", nmix)
        nms <- names(mix)
        numid <- 0
  
        for (i in seq_along(mix)){
            m <- parseMix(mix[[i]], nms[i], numid=numid)
            numid <- m$numid
            mixl[[i]] <- m
            mixcat[[i]] <- m$cat
            mixnum <- c(mixnum, m$nums)
        }

        # combine categorical edits, datamodel with dummies for mixed edits and mixed edits
        mix <- c(
            ind2char(getInd(cat)),
            if ( length(mixnum) > 0 ) paste(names(mixnum), "%in% c(TRUE,FALSE)"),
            as.character(mixcat)
        )
    }

    # stick categorical and mixed edits in one editarray
    mixcat <- editarray(mix, env=env)
    nmix <- nrow(mixcat)
    if ( nmix>0 ){
        rownames(mixcat) <- paste("mix",(nnum+ncat+1):(nnum+ncat+nmix),sep="")
        mixcat <- c(cat, mixcat)
    } else {
        mixcat <- cat
    }

    # create editmatrix for mixed edits and name them with dummy variable names
    nms <- names(mixnum)
    mixnum <- editmatrix(as.character(mixnum))
    rownames(mixnum) <- nms
  
    neweditset(
        num=num,
        mixcat=mixcat,
        mixnum=mixnum
    )
}

#
#
#
neweditset <- function(num, mixcat, mixnum,...){

  structure(
      list( num = num
          , mixnum = mixnum
          , mixcat = mixcat
          )
    , class="editset" 
    , ...
  )

}



#' Add dummy variable to the data.frames, these are needed for errorlocations etc.
#' @param E editset
#' @param dat data.frame
#' @return data.frame with dummy variables added
#' @keywords internal
adddummies <- function(E, dat){
  dummies <- !violatedEdits(E$mixnum, dat)
  cbind(dat, dummies)
}



#' Convert an editset to character
#' @method as.character editset
#'
#' @param x an \code{\link{editset}}
#' @param datamodel include datamodel?
#' @param useIf return vectorized version?
#' @param dummies return datamodel for dummy variables?
#' @param ... arguments to be passed to or from other methods
#' @export
as.character.editset <- function(x, datamodel=TRUE, useIf=TRUE, dummies=FALSE, ...){
    num <-  as.character(x$num)
    numc <- as.character(x$mixnum)
    catc <- as.character(x$mixcat, datamodel=datamodel, useIf=useIf)
    for ( n in names(numc) ){
        catc <- catc[!grepl(paste(n,'%in%'),catc)]
        catc <- gsub(paste(n,'== FALSE'), invert(numc[n]),catc)
        catc <- gsub(paste(n,'== TRUE'), numc[n],catc)
    }
    # remove datamodel which are a consequence of conditional edits
    
    c(num,catc)
}


# invert a textual numerical edit
invert <- function(e){
    gte   <- grep(">=",e)
    gt    <- grep(">",e) & !gte
    lte   <- grep("<=",e)
    lt    <- grep("<",e) & !lte
    eq    <- grep("==",e)
    ineq  <- grep("!=",e)
    e[gte]  <- gsub(">=","<",e[gte])
    e[gt]   <- gsub(">","<=",e[gt])
    e[lte]  <- gsub("<=",">",e[lte])
    e[lt]   <- gsub("<",">=",e[lt])
    e[ineq] <- gsub("!=","==",e[ineq])
    e[eq]   <- gsub("==","!=",e[eq])
    e
}



#' Coerce an editarset to a \code{data.frame}
#'
#' Coerces an editset to a \code{data.frame}. 
#'
#' @method as.data.frame editset
#' @param x \code{\link{editset}} object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{as.character.editarray}}
#' @return data.frame with columns 'name', 'edit' and 'description'.
#'
#' @export 
as.data.frame.editset <- function(x, ...){
    edts <- as.character(x, datamodel=TRUE,...)
    d <- data.frame(
        name=names(edts),
        edit=edts,
        row.names=NULL,
        stringsAsFactors=FALSE
    )
    if (!is.null(attr(x,'description'))) d$description <- attr(x,'description')
    d
}






## quick test
# es <- editset(expression(if (x > 0) y + 1 < 20
#                         , x <= 100
#                         , if (x < 10) y >= 2
#                         , A %in% c("a1", "a2")
#                         , B %in% c("b1", "b2")
#                         , if (A == "a1") B == "b2"
#                         , if (y > 0) A == "a2"
#                         )
#              )
# #es
# 
# dat <- data.frame(x=1:2, y=10:9, A="a1", B="b2")
# #adddummies(es,dat)
# 
# violatedEdits(es, dat)
