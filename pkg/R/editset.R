
editset <- function(editrules){
  #if (is.null(names(editrules)))
  #  names(editrules) <- paste("me", seq_along(editrules), sep="")
  
  num <- parseEdits(editrules, type="num")
  cat <- parseEdits(editrules, type="cat")
  mix <- parseEdits(editrules, type="mix") 
  
  nmix <- length(mix)
  if (is.null(names(mix))) names(mix) <- paste("mix", seq_along(mix), sep="")
  
  num <- if (length(num) > 0) editmatrix(num)
  cat <- if (length(cat) > 0) editarray(cat)
  
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
  
  mixcat <- editarray(mixdatamodel)
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

print.editset <- function(x, ...){
  attr(x, "parseMix") <- NULL
  print(unclass(x))
}

## quick test
es <- editset(expression(if (x > 0) y + 1 < 2
                        , x <= 100
                        , if (x < 10) y >= 2
                        , A %in% c("a1", "a2")
                        , B %in% c("b1", "b2")
                        , if (A == "a1") B == "b2"
                        , if (y > 0) A == "a2"
                        )
             )
es
