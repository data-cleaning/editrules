MIXOPS <- c("if", "||", "|", "&&", "&")

#' Parse a mixed edit
#'
#' parseMix replaces all numerical edits with a generated dummy boolean variable and returns the resulting categorical
#' edit plus the list of found of numerical edits. These exppression should be handled further by \code{parseCat} and 
#' \code{parseNum}.
#' @param e expression to be parsed
#' @param numid starting number for dummy name generation
#' @return list with categorical expression (\code{cat}) and a numerical expression (code{nums})
#' @keywords internal
parseMix <- function(e, editname="", numid=0){
  
  # should the expressions be returned or should parseCat and parseNum be called on cat and nums?
  
  if (length(e) < 3) return(NULL)
  op <- as.character(e[[1]])
  if (!op %in% MIXOPS) stop("invalid mix")
  
  cat <- e
  nums <- expression()
  numid <- numid
  
  pm <- function(i){
    # rewrite equality as inequalities
    #e[[i]] <- rewriteEq(e[[i]])
    if (isNum(e[[i]])){
      numid <<- numid + 1
      numvar <- paste(editname, ".num.",numid,sep="")
      #replace numeric edit with generated dummy boolean edit name
      cat[[i]] <<- as.name(numvar)
      nums[numvar] <<- as.expression(e[[i]])
    } else if (!isCat(e[[i]])){
      l <- parseMix(e[[i]], numid=numid, editname=editname)
      cat[[i]] <<- l$cat
      nums[names(l$nums)] <<- l$nums
      numid <<- l$numid
    }  
  }
  pm(2)
  pm(3)
  
  # orNums is the or for for numerical edits, some will be negated.
  
  orNums <- nums
  numDummies <- names(orNums)
  neg <- which(!parseCat(cat, useLogical=TRUE)[numDummies])
  orNums[neg] <- sapply(orNums[neg], negateEdit)
  
  list( cat  = cat # parseCat(cat)
      , nums = nums # lapply(nums, parseNum)
      , orNums = orNums # lapply(orNums, parseNum)
      , numid=numid 
      )
}

# e has to be an edit
negateEdit <- function(e){
  op <- as.character(e[[1]])
  if (op == "!") 
    return(e[[2]])
  op <- switch( op
              , "<"  = ">="
              , "<=" = ">"
              , ">"  = "<="
              , ">=" = "<"
              , "==" = "!="
              , "!=" = "=="
              )
  if (is.null(op)){
    ne <- quote(!a)
    ne[[2]] <- e
    e <- ne
  } else {
    e[[1]] <- as.symbol(op)
  }
  e
}

# e has to be an numerical inequality!
rewriteInEq <- function(e){
  op <- as.character(e[[1]])
  if (op != "!=") return(e)
  eAnd <- quote(a || b)
  eAnd[[2]] <- e
  eAnd[[2]][[1]] <- as.symbol(">")
  eAnd[[3]] <- e
  eAnd[[3]][[1]] <- as.symbol("<")
  eAnd
}

## quick test
# rewriteInEq(quote(x != y + 1))
# 
# a <- negateEdit(quote(x>2))
# a
# negateEdit(a)
# 
# a <- negateEdit(quote(A %in% "a"))
# a
# negateEdit(a)
# 
# pm <- parseMix(quote(if(x>1 && x < 10 && A %in% c('a1')) y > 2), editname="e1")
# pm
# 
# 
