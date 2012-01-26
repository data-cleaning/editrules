#' Create an editset which can contain a mix of categorical, numerical and mixededits
#' 
#' NOTE: at the moment, functionality for mixed edit sets is limited and somewhat experimental.
#'
#' @param editrules \code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
#'        a \code{character} or \code{expression} with (in)equalities written in R syntax
#' @param env environment to parse categorical edits in
#' @export
editset <- function(editrules, env=new.env()){
  #if (is.null(names(editrules)))
  #  names(editrules) <- paste("me", seq_along(editrules), sep="")
  
  num <- parseEdits(editrules, type="num")
  cat <- parseEdits(editrules, type="cat")
  mix <- parseEdits(editrules, type="mix")
  
  nmix <- length(mix)
  if (is.null(names(mix))) names(mix) <- paste("mix", seq_along(mix), sep="")
  
  num <- if (length(num) > 0) editmatrix(num)
  if (length(cat) > 0){ 
    cat <- editarray(cat,env=env)
    rownames(cat) <- paste("e",(nrow(num)+1):(nrow(num)+nrow(cat)),sep="")
  }

  
  mixl <- vector(mode="list", nmix)
  mixcat <- vector(mode="expression", nmix)
  mixnum <- expression()
  nms <- names(mix)
  numid <- 0
  
  for (i in seq_along(mix)){
    m <- parseMix(mix[[i]], nms[i], numid=numid)
    numid <- m$numid
    mixl[[i]] <- m
    mixcat[[i]] <- m$cat
    mixnum <- c(mixnum, m$nums)
  }
  
  # add datamodel for categorical edits, add datamodel for dummy variables and add mixed categorical edits
  mixdatamodel <- c( if (!is.null(cat)) ind2char(getInd(cat))
                   , paste(names(mixnum), "%in% c(FALSE,TRUE)")
                   , as.character(mixcat)
                   )
  
  mixcat <- editarray(mixdatamodel, env=env)
  rownames(mixcat) <- names(mix)
  #mixnum <- editmatrix(mixnum)
  
  # create editmatrix for mixed edits and name them with dummy variable names
  nms <- names(mixnum)
  mixnum <- editmatrix(as.character(mixnum))
  rownames(mixnum) <- nms
  
  # the numeric matrix might miss numeric variables used in mixed edits, so add empty columns for these variables
  missvars <- setdiff(getVars(mixnum), getVars(num))
  missvars <- matrix(0, ncol=length(missvars), nrow(num), dimnames=list(NULL, missvars))
  A <- cbind(getA(num), missvars)
  num <- as.editmatrix(A, getb(num), ops=getOps(num))
  
  structure(
      list( num = num
          , cat = cat
          , mixnum = mixnum
          , mixcat = mixcat
          )
    , class="editset" # maybe mixEdits?
    , parseMix = mixl
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
#' @param ... arguments to be passed to or from other methods
#' @export
as.character.editset <- function(x, datamodel=TRUE,useIf=TRUE,...){
    num <-  as.character(x$num)
    cat <-  as.character(x$cat,datamodel=datamodel,useIf=useIf)
    numc <- as.character(x$mixnum)
    catc <- as.character(x$mixcat,datamodel=FALSE,useIf=useIf)
    for ( n in names(numc) ){
        catc <- gsub(paste(n,'== FALSE'), invert(numc[n]),catc)
        catc <- gsub(paste(n,'== TRUE'), numc[n],catc)
    }
    # remove datamodel which are a consequence of conditional edits
    
    c(num,cat,catc)
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
    e[eq]   <- gsub("==","!=",e[ineq])
    e
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
