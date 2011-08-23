#' Create an editmatrix with categorical variables
#'
#' @param x \code{character} with categorical edits
#' @return cateditmatrix object, which is a specialized \code{\link{editmatrix}}
cateditmatrix <- function(x){
    edts <- parseEdits(x)
    
    catedits <- lapply(edts, parseCatEdit)
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

#' get all variables with its categories
getVarCat <- function(x, ...){
    UseMethod("getVarCat")
}

#' @method getVarCat cateditmatrix
getVarCat.cateditmatrix <- function(E, ...){
   getVarCat(getVars(E), ...)
}

#' @method getVarCat editmatrix
getVarCat.editmatrix <- function(E, ...){
   getVarCat(getVars(E), ...)
}

#' @method getVarCat character
getVarCat.character <- function(x, ...){
   
   var <- gsub(":.+", "", x)
   cat <- gsub(".+:", "", x)
   cat[var==cat] <- "TRUE"
   return(data.frame(var=var, cat=cat, fullname=x, stringsAsFactors=FALSE))
}

negateValue <- function(x, variable, value){
   neg <- numeric(ncol(x))
   names(neg) <- colnames(x)
   
   if (is.logical(value)){
     neg[variable] <- 1
     neg[ncol(x)]<- !value
   } else {
      vc <- subset(getVarCat(x), var==variable & cat==value)
      #TODO multiple negations
      neg[vc$fullname] <- 1
      neg[ncol(x)] <- 0
      
   }
   Ab <- rbind(getAb(x), neg)
   rownames(Ab)[nrow(Ab)] <- paste("not",variable,value, sep="_")
   
   E <- editrules:::neweditmatrix( Ab
                                 , c(getOps(x),"==")
                                 )
   class(E) <- c("cateditmatrix", class(E))
   E
}

substValue.cateditmatrix <- function(E, variable, value){
   vc <- getVarCat(E)
   vc <- vc[vc$var %in% variable,]
   if (!nrow(vc))
      stop("Invalid variable: ", var)
   
   for (v in variable){
       vcv <- vc[vc$var==v,]
       values <- as.numeric(vcv$cat %in% value)
       names(values) <- vcv$cat
       E <- substValue(E, vcv$fullname, values, remove=TRUE)
   }
   class(E) <- c("cateditmatrix", class(E))
   # rngs <- ranges(E)
   # eq <- rngs[,1] == rngs[,2]
   # if (any(eq)){ 
      # ops <- getOps(E)
      # ops[eq] <- "=="
      # attr(E, "ops") <- ops
   # }
   E
}

eliminateFM.cateditmatrix <- function(E, variable){
   vars <- subset(getVarCat(E), var==variable)$fullname
   for (v in vars) E <- eliminateFM(E,v)
   class(E) <- c("cateditmatrix", class(E))
   E
}

#' finds the min and max value of an categorical edit
#' 
#' This can be used to check for infeasibility or redundancy
#' @param E categorical editmatrix
#' @return \code{numeric matrix} with columns min and max
ranges <- function(E){
  A <- getA(E)
  vars <- getVarCat(E)$var
  t(apply(   A
           , 1
           , function(r){
              ub <- tapply(r, vars, max)
              lb <- tapply(r, vars, min)
              c(min=sum(lb[lb<0]), max=sum(ub[ub>0]))
             }
         )
    )
}

isObviouslyRedundant.cateditmatrix <- function(E){
   (  (getOps(E) %in% c("<=","<") & (getb(E) >= ranges(E)[,"max"]))
   |  editrules:::isObviouslyRedundant.editmatrix(E)
   )
}

#'check which edits are infeasible
isObviouslyInfeasible.cateditmatrix <- function(E){
   nrow(E) && any(getb(E) < ranges(E)[,"min"])
}

errorLocalizer.cateditmatrix <- function(E, x, weight=rep(1,length(x)),...){
    # store class for backcasting
    cemclass <- class(E)
    
    if ( !isNormalized(E) ) E <- normalize(E)
    
    # missings must be adapted, others still have to be treated.
    adapt <- is.na(x)   
    names(adapt) <- names(x)

    #order decreasing by weight
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]

    # Eliminate missing variables.
    vars <- getVars(E)
    for (v in names(which(adapt))) E <- eliminateFM.cateditmatrix(E,v)
    
    wsol <- sum(weight)
    cp <- backtracker(
        isSolution = {
            w <- sum(weight[adapt])
            if ( w > wsol || isObviouslyInfeasible.cateditmatrix(.E)  ) return(FALSE)
            if (length(.totreat) == 0){
                wsol <<- w
                adapt <- adapt 
                return(TRUE)
            }
        },
        choiceLeft = {
            .var <- .totreat[1]
            .E <- substValue.cateditmatrix(.E, .var , x[.var])
            .totreat <- .totreat[-1]

            adapt[.var] <- FALSE
        },
        choiceRight = {
            .var <- .totreat[1]
            #.E <- negateValue(.E, .var, x[.var])
            .E <- eliminateFM.cateditmatrix(.E, .var)
            .totreat <- .totreat[-1]
            adapt[.var] <- TRUE
        },
        .E = E,
        .totreat = totreat,
        x = x,
        adapt = adapt,
        weight = weight,
        wsol = wsol 
    )
}

#' parse categorial edit

#' @param e \code{expression} with a single edit
#' @return named \code{numeric} with coefficients
parseCatEdit <- function(e){
  el <- parseCat(e, useLogical=TRUE)
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
# ### examples....

civilStatusLevels <- c("married","unmarried","widowed","divorced")

x <- c( "if (positionInHousehold == 'marriage partner') civilStatus == 'married'"
      , "if (age == '< 16') civilStatus=='unmarried'"
#      , "civilStatus %in% civilStatusLevels" #that looks magical, but civilstatusLevels is evaluated
      , "if (pregnant) gender == 'female'"
      , "if (nace %in% c('A','B')) valid==TRUE"
      , "gender %in% c('male','female')"
      )

(E <- cateditmatrix(x))