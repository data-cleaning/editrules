
expand <- function(s, prefix="_", useSum=FALSE, ...){  
  l <- list(...)
  
  varnms <- paste(prefix,names(l), sep="")
  
  if (useSum) {
    sumnms <- paste("sum", names(l), sep=prefix)
    sumregex1 <- paste(sumnms, "\\((.+?)\\)", sep="")
    sumregex2 <- paste(sumnms, "\\((.+?)\\).+", sep="")
    vars <- names(l)
    print(sumnms)
    for (i in seq_along(vars)){      
      if (grep(sumregex2[i], s)){
        sumvars <- sub(sumregex2[i], "\\1", s)
        sumvars <- do.call(expand, append(list(s=sumvars), l))
        sumvars <- paste(sumvars, collapse=" + ")
        #TODO iterate and remove index from list
        s <- sub(sumregex1[i], sumvars, s)
        l[[vars[i]]] <- NULL
      }
    }
  }
  
  
  for (i in seq_along(l)){
    s <- sapply(l[[i]], function(j) gsub(varnms[i],j,s))    
  }
  
  if (is.array(s)) {
    dimnames(s) <- l
  } else if (is.vector(s)){
    names(s) <- l[[1]]
  }
  s
}

# quick test
expand("x_i < y_j", i=1:3,j=2:3)
expand("sum_i(x_i) == y", i=1:3, useSum=TRUE)

# sumvar <- sub("sum_i\\((.+?)\\).+", "\\1", "sum_i(x_i) == y")
# sumvar <- paste(expand(sumvar, i=1:10), collapse=" + ")
# sumvar
# sub("sum_i\\((.+?)\\)", sumvar, "sum_i(x_i) == y")