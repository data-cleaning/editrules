library(editrules)
FILE =" benchmip_categorical.txt"

generate_E <- function(nvar=10){
  if (nvar < 1) stop("nvar needs to be bigger than 3")
  s <- seq_len(nvar)
  
  var_num <- head(s, ceiling(nvar/2))
  var_cat <- tail(s, floor(nvar/2))
  n_cat <- length(var_cat)
  
  var_num <- paste0("n", var_num)
  if (n_cat) var_cat <- paste0("C", var_cat-n_cat) else character()
  
  if (length(var_num) > 1){
    nsum <- paste(tail(var_num, -1), collapse="+")
    edits <- paste0("n1 == ", nsum)
  } else {
    edits <- "n1 == 0"
  }
  
  if (n_cat){
    edits <- c( edits
              , paste0(var_cat, " %in% c(TRUE,FALSE)")
              , paste0("if (", var_cat, ") ", tail(var_num,n_cat),"<=0")
              )
  }
  editset(edits)
}

generate_data <- function(E, nerrors=0){
  num_vars <- getVars(E,"num")
  cat_vars <- getVars(E, "cat")
  
  x_cat <- sapply(cat_vars, function(v) TRUE)
  x_num <- sapply(num_vars, function(v) 0)
  x1 <- x
  x1[seq_len(nerrors)] <- FALSE
  
  x2 <- x
  x2[1+n - seq_len(nerrors)] <- FALSE
  
  x3 <- x
  x3[round((n-nerrors)/2) + seq_len(nerrors)] <- FALSE
  as.data.frame(rbind(x1,x2,x3))
}


bench <- function(nvars = 10, nerrors=10, method="bb"){
  
  init <- !file.exists(FILE)
  txt <- file(FILE, "at")
  on.exit(close(txt))
  
  errorloc <- c("begin", "end", "middle")
  
  if (nerrors >= nvars) stop("nvars cannot be less than nerrors")
  for (ne in seq_len(nerrors)){
    for (nvar in seq(from=ne+1, to=nvars)){
      E <- generate_E(nvar)
      data <- generate_data(E, ne)
      cat("\r nvar=", nvar, " ne=", ne, " method=", method)
      le <- localizeErrors(E, data, method=method)
      rpt <- cbind(method=method, nvar=nvar, nerrors=ne, errorloc=errorloc, le$status)
      
      write.table(rpt, file=txt, col.names=init, row.names=FALSE)
      init <- FALSE
      flush(txt)
      #print(rpt)
    }
  }
}

# if (file.exists(FILE)) file.remove(FILE)
# 
# bench(50,10, method="mip")
# bench(50,10, method="bb")
# 
# dat <- read.table(FILE, header=TRUE)
# library(ggplot2)
# qplot(data=dat, y=elapsed, x=nvar, color=method, facets=nerrors~method, shape=errorloc) + geom_jitter()
# ggsave("benchmip_categorical.png")


### quick testing
(E <- generate_E(3))
generate_data(E, nerrors=1)
