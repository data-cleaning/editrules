#' Create an editmatrix with categorical variables
#'
#' \code{cateditmatrix} is an alternate representation of a categorial edit. 
#' The prefered representation is \code{editarray}, but cateditmatrix is useful for
#' transforming solving categorical edit into a mixed integer programming problem
#'
#' @param x \code{character} with categorical edits
#' @return cateditmatrix object, which is a specialized \code{\link{editmatrix}}
#' @keywords internal
cateditmatrix <- function(x, sep=":"){
    if (is.editarray(x)) {
      x <- as.character(x,datamodel=FALSE)
    }
    edts <- parseEdits(x)
    
    catedits <- lapply(edts,parseCat,sep=sep, useLogical=TRUE)
    catedits <- lapply(catedits, parseCatEdit)
    
    categories <- sort(unique(names(unlist(catedits))))
    categories <- c(categories[categories!="b"],"b")

    A <- matrix( 0
               , ncol=length(categories)
               , nrow=length(catedits)
               , dimnames = list( rules = names(catedits)
                                , cats=categories
                                )
               )
                     
    for (i in seq_along(catedits)){
           A[i,names(catedits[[i]])] <- catedits[[i]]
    }

    b <- A[,ncol(A)]
    A <- A[,-ncol(A), drop=FALSE]
    
    ops <- sapply(edts, function(e){deparse(e[[1]])})
    ops <- ifelse(ops %in% c("if", "||"), "<=", "==")

    E <- as.editmatrix(A,b,ops)
    class(E) <- c("cateditmatrix", "editmatrix")
    E
}

#' parse categorial edit

#' @param e \code{expression} with a single edit
#' @return named \code{numeric} with coefficients
#' @keywords internal
parseCatEdit <- function(el){
  #el <- parseCat(e, useLogical=TRUE)
  if (any(is.na(el))){
    val <- rep(1, length(el)+1)
    names(val) <- c(names(el), "b")
  } else {
    vars <- gsub(":.+","",names(el))
    # coefficients in form 
    val <- ifelse(el, 1, -1)
    m <- tapply(val, vars, max)
    b <- sum(m[m>0]) - 1
    val <- c(val, b=b)
  }
  val
}

 ### examples....

# #civilStatusLevels <- c("married","unmarried","widowed","divorced")
# # 
# x <- c( "if (positionInHousehold == 'marriage partner') civilStatus == 'married'"
#       , "if (age == '< 16') civilStatus=='unmarried'"
# #      , "civilStatus %in% civilStatusLevels" #that looks magical, but civilstatusLevels is evaluated
#       , "if (pregnant) gender == 'female'"
#       , "if (nace %in% c('A','B')) valid==TRUE"
#       , "gender %in% c('male','female')"
#       )
# 
# (E <- cateditmatrix(x))
