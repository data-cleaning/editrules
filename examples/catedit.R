parseEdits <- function(x){
   parse(text=x)
}


parseTree <- function(expr,prefix=NULL){
   if (length(expr) == 1){
      indent <- paste("[", prefix,"]", sep="", collapse="")
      cat(indent, expr,"\n")
   }
   else {
       for (i in 1:length(expr)){
          parseTree(expr[[i]], c(prefix,i)) 
       }
   }
}

getCondition <- function(var, value, pos){
}

parseCond2 <- function(cond, pos=1, l=list()){
   op <- as.character(cond[[1]])
   #TODO add checks for && and ||
   if (op == "if"){
      l <- parseCond(cond[[2]], -pos, l)
      l <- parseCond(cond[[3]], pos, l)
   }
   else if (op == "!"){
      l <- parseCond(cond[[2]], -pos, l)
   }
   # TODO check if it is a categoral or a numerical constraint
   # i.e. '==' should be disambigued
   else if (op %in% c("==", "%in%")){
      var <- as.character(cond[[2]])
      value <- eval(cond[[3]])
      l <- append(l, list(list(var=var, value=value, pos=pos)))
   }
   l
}

parseCond <- function(cond, pos=1, l=numeric(0)){
   op <- as.character(cond[[1]])
   #TODO add checks for && and ||
   if (op == "if"){
      l <- parseCond(cond[[2]], -pos, l)
      l <- parseCond(cond[[3]], pos, l)
   }
   else if (op == "!"){
      l <- parseCond(cond[[2]], -pos, l)
   }
   # TODO check if it is a categoral or a numerical constraint
   # i.e. '==' should be disambigued
   else if (op %in% c("==", "%in%")){
      var <- as.character(cond[[2]])
      cat <- eval(cond[[3]])
      value <- rep(pos, length(cat))
      names(value) <- paste(var,":",cat,sep="")
      l <- c(l, value)
   }
   l
}

civilStatus <- c("married","unmarried","widowed","divorced")

x <- c( "if (positionInHousehold == 'marriage partner') civilStatus == 'married'"
      , "if (age == '< 16') civilStatus=='unmarried'"
      , "civilStatus %in% civilStatus"
      #, "if (pregnant == TRUE) gender == 'female'"
      #, "if (nace %in% c('A','B')) valid==TRUE"
      )
      
edts <- parseEdits(x)
for (i in 1:length(edts)){
    parseTree(edts[[i]])
}

lapply(edts, parseCond)
# pc <- parseCond(edts[[2]])
# parseCond(edts[[1]])
