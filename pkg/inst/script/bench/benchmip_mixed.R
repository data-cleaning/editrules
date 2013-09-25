library(editrules)
FILE ="benchmip_mixed.txt"

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
    edits <- c( "n1 >= 0"
              , edits
              , paste0(var_cat, " %in% c(TRUE,FALSE)")
              , paste0("if (!", var_cat, ") ", tail(var_num,n_cat),"< 0")
              )
  }
  editset(edits)
}

error <- function(x){
  lapply(x, function(v){
    if (is.logical(v)) FALSE else -1
  })
}

generate_data <- function(E, nerrors=0){
  num_vars <- getVars(E,"num")
  
  x <- sapply(getVars(E), function(v){
    if (v %in% num_vars) 0 else TRUE
  }, simplify=FALSE)
  
  n <- length(x)
    
  x1 <- x
  idx <- seq_len(nerrors)
  x1[idx] <- error(x1[idx])
  
  x2 <- x
  idx <- 1+n - seq_len(nerrors)
  x2[idx] <- error(x2[idx])
  
  x3 <- x
  idx <- (round(n-nerrors)/2) + seq_len(nerrors)
  x3[idx] <- error(x3[idx])
  
  rbind(as.data.frame(x1),x2,x3)
}


bench <- function(nvars = 10, nerrors=10, method="bb"){
  
  init <- !file.exists(FILE)
  txt <- file(FILE, "at")
  on.exit(close(txt))
  
  errorloc <- c("begin", "end", "middle")
  
  if (nerrors >= nvars) stop("nvars cannot be less than nerrors")
  for (nvar in seq_len(nvars)){
    for (ne in seq(1, min(nerrors, nvar))){
      try({
        E <- generate_E(nvar)
        data <- generate_data(E, ne)
        cat("\r nvar=", nvar, " ne=", ne, " method=", method)
        le <- localizeErrors(E, data, method=method)
        rpt <- cbind(method=method, nvar=nvar, nerrors=ne, errorloc=errorloc, le$status)
        
        write.table(rpt, file=txt, col.names=init, row.names=FALSE)
        init <- FALSE
        flush(txt)
      #print(rpt)
      })
      gc()
    }
  }
}

if (file.exists(FILE)) file.remove(FILE)

bench(100,10, method="mip")
bench(50,10, method="bb")

dat <- read.table(FILE, header=TRUE)
library(ggplot2)
qplot(data=dat, y=elapsed, x=nvar, color=method, facets=nerrors~method, shape=errorloc) + geom_jitter()
ggsave("benchmip_categorical.png")




### quick testing
# E <- generate_E(2)
# generate_data(E, nerrors=1)
