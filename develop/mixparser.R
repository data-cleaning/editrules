NUMCMP <- c("==","<","<=",">",">=")
NUMOPS <- c("+","-","*")
CATCMP <- c("==", "!=", "%in%")

hasNum <- function(e){
  if (length(e) == 1){
    return(is.numeric(e))
  }
  op <- deparse(e[[1]])
  if (length(e) == 2){
    return (op %in% NUMOPS || hasNum(e[[2]]))
  }
  if (length(e) == 3){
    return(op %in% NUMOPS || hasNum(e[[2]]) || hasNum(e[[3]]))
  }
}

editTypes <- function(edts){
  ops <- sapply(edts, function(e){deparse(e[[1]])})
  
  type  <- ifelse(ops %in% NUMCMP, "num", "cat")
  # todo add check for "=="

  iff <- ops == "if"
  mix <- sapply(edts[iff], hasNum)
  type[iff] <- ifelse(mix, "mix", "cat")
  as.factor(type)
}

x <- c( "2*x < 1"
      , "if (A=='a') B == 'b'"
      , "if (A=='a') B == FALSE"
      , "if (A=='a') B > 1"
      , "if (c==1) B || C == FALSE"
      )

#' Parse a character vector of edits
#'
#' This function wraps the native \code{\link{parse}} function in a \code{\link{tryCatch}}.
#' The function is \code{editrules} internal. It tries to give a meaningfull error message when
#' parsing fails for some reason.
#'
#' @param E \code{character}
#' @return The edits in \code{E} parsed to R expressions.
#'
parseEdits <- function(E, type=c("all", "num", "cat", "mix")){
     edits <- 
        tryCatch(parse(text=E), 
            error=function(e){
                stop(paste("Not all edits can be parsed, parser returned", e$message,sep="\n"))
            })
     type <- match.arg()
     if (type=="all"){
       return(edits)
     }
     else return(edits[editTypes(edits) == type])
}

parseEdits(x)