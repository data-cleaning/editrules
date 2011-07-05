require(editrules)

 parseEdits <- function(x){
    parse(text=x)
 }


# parseTree <- function(expr,prefix=NULL){
   # if (length(expr) == 1){
      # indent <- paste("[", prefix,"]", sep="", collapse="")
      # cat(indent, expr,"\n")
   # }
   # else {
       # for (i in 1:length(expr)){
          # parseTree(expr[[i]], c(prefix,i)) 
       # }
   # }
# }

parseCond <- function(cond, pos=1, l=c(b=1), iscond=FALSE){
   if (length(cond) == 1){
      value = pos
      names(value) <- as.character(cond)
      if (!iscond){
         l["b"] <- 1
      }
      if (pos < 0){
         l["b"] <- l["b"] - 1
      }
      l <- c(l, value) 
      return(l)
   }
   op <- as.character(cond[[1]])
   #TODO add checks for && and ||
   if (op == "if"){
      l <- parseCond(cond[[2]], -pos, l, iscond=TRUE)
      l <- parseCond(cond[[3]], pos, l, iscond=TRUE)
   }
   else if (op %in% c("!", "{","(")){
      if (op == "!") pos <- -pos
      l <- parseCond(cond[[2]], pos, l, iscond=iscond)
   }
   # TODO check if it is a categoral or a numerical constraint
   # i.e. '==' should be disambigued
   else if (op %in% c("==", "%in%","!=")){
      if (op == "!=") pos <- -pos
      var <- as.character(cond[[2]])
      cat <- eval(cond[[3]])
      if (is.logical(cat)){
         value <- ifelse(cat, pos, -pos)
         names(value) <- var
      } else {
          value <- rep(pos, length(cat))
          names(value) <- paste(var,":",cat,sep="")
      }
      if (!iscond){
         l["b"] <- 1
      }
      if (pos < 0){
         l["b"] <- l["b"] - 1
      }
      l <- c(l, value)
   }
   else if (op == "&&"){

         if (pos > 0){
            stop("Invalid use of ", op, " in then clause.")
         }         
         l <- parseCond(cond[[2]], pos, l, iscond=iscond)
         l <- parseCond(cond[[3]], pos, l, iscond=iscond)
       }
   }
   else if (op == "||"){
         if (pos < 0){
            stop("Invalid use of ", op, " in if clause.")
         }         
         l <- parseCond(cond[[2]], pos, l, iscond=iscond)
         l <- parseCond(cond[[3]], pos, l, iscond=iscond)
   }
   else {
      stop("Operator ", op, " not supported.")
   }
   l
}

#' Create an editmatrix with categorical variables
#'
#' @param x \code{character} with categorical edits
#' @return cateditmatrix object, which is a specialized \code{\link{editmatrix}}
cateditmatrix <- function(x){
    edts <- parseEdits(x)
    
    catedits <- lapply(edts, parseCond)
    categories <- unique(names(unlist(catedits)))
    categories <- c(categories[categories!="b"],"b")

    A <- matrix( 0
               , ncol=length(categories)
               , nrow=length(catedits)
               , dimnames = list( rules = NULL
                                , cats=categories
                                )
               )
                     
    for (i in seq_along(catedits)){
           A[i,names(catedits[[i]])] <- catedits[[i]]
    }

    b <- A[,ncol(A)]
    A <- A[,-ncol(A), drop=FALSE]
    
    ops <- sapply(edts, function(e){deparse(e[[1]])})
    ops <- ifelse(ops %in% c("if", "||"), ">=", "==")

    E <- normalize(as.editmatrix(A,b,ops))
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

#' @method getVarCat character
getVarCat.character <- function(x, ...){
   vc <- as.data.frame(t(sapply(strsplit(x, ":")
                               , function(vc) c(var=vc[1], cat=vc[2])
                               )
                        )
                      , stringsAsFactors=FALSE
                      )
   # set value of logical variables to TRUE
   vc$cat[is.na(vc$cat)] <- "TRUE"
   vc$fullname <- x
   vc
}

#' get all variables with its categories
#' @method as.character cateditmatrix
#' @param E \code{cateditmatrix} object
#' @return \code{character} with the character representation of the edits
# as.character.cateditmatrix <- function(E, ...){
    # A <- getA(E)
    # ops <- getOps(E)
    
    # ifc <- A > 0 & ops == "<="
    # thenc <- A < 0 | ( A > 0 & ops == "==")
    
    # #generate %in% statement
    # inclause <- function(varcat, collapse=NULL){
       # vc <- getVarCat(varcat)
       # vc <- split(vc$cat, vc$var)
       # vc <- sapply( names(vc)
                   # , function(var){
                        # cats <- vc[[var]]
                        # if (length(cats) == 1){ #this is for a logical variable
                           # if (cats %in% TRUE)
                               # return(var)
                           # paste(var," == '",cats,"'", sep="")  # cosmetic, for one category we generate an "==" statement
                        # } else {
                            # cats <- paste("'",cats,"'", sep="", collapse=",")
                            # paste(var," %in% c(",cats,")", sep="")
                        # }
                     # }
                   # )
       # paste(vc, collapse=collapse)
    # }
    
    # vars <- getVars(E)
    # catedits <- rownames(E)
    # for (i in 1:length(catedits)){
        # if (any(thenc[i,])){
            # thenvars <- inclause(vars[thenc[i,]], collapse=" || ")
            # if (any(ifc[i,])){
                # ifvars <- inclause(vars[ifc[i,]], collapse=" && ")
                # catedits[i] <- paste("if (",ifvars,") ",thenvars, sep="")
            # }
            # else {
                # catedits[i] <- thenvars
            # }
        # } else{
           # catedits[i] <- paste("!(", inclause(vars[ifc[i,]]), ")",sep="",collapse=" || ")
        # }
    # }
    # names(catedits) <- rownames(E)
    # catedits
# }

substValue.cateditmatrix <- function(x, var, value){
   vc <- getVarCat(x)
   vc <- vc[vc$var == var,]
   if (!nrow(vc))
      stop("Invalid variable: ", var)
   
   values <- as.numeric(vc$cat %in% value)
   names(values) <- vc$cat
   
   E <- substValue(x, vc$fullname, values, remove=TRUE)
   
   class(E) <- class(x) 
   # rngs <- ranges(E)
   # eq <- rngs[,1] == rngs[,2]
   # if (any(eq)){ 
      # ops <- getOps(E)
      # ops[eq] <- "=="
      # attr(E, "ops") <- ops
   # }
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
  t(apply( A
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
   (getb(E) >= ranges(E)[,"max"]) | editrules:::isObviouslyRedundant.editmatrix(E) 
}

#'check which edits are infeasible
isObviouslyInfeasible.cateditmatrix <- function(E){
   getb(E) < ranges(E)[,"min"]
}

# ### examples....

civilStatusLevels <- c("married","unmarried","widowed","divorced")

x <- c( "if (positionInHousehold == 'marriage partner') civilStatus == 'married'"
      , "if (age == '< 16') civilStatus=='unmarried'"
      , "civilStatus %in% civilStatusLevels" #that looks magical, but civilstatusLevels is evaluated
      , "if (pregnant) gender == 'female'"
      , "if (nace %in% c('A','B')) valid==TRUE"
      )


E <- cateditmatrix(x)
E
as.character(E)
getVarCat(E, name="variable")

E <- substValue.cateditmatrix(E, "civilStatus", "unmarried")
E

E <- substValue.cateditmatrix(E, "gender", "male")
E


E <- substValue.cateditmatrix(E, "pregnant", TRUE)
E

cateditmatrix(c("if (A=='a' && B=='b') C=='c'"))
cateditmatrix(c("if (A=='a') {B == 'b' || C=='c'}"))
#ranges(E)
#redundant(E)
isObviouslyInfeasible.cateditmatrix(E)
E