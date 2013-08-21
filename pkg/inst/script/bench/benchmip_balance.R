library(editrules)
FILE ="benchmip_balance.txt"

#' Create account balance
generate_balance <- function(nvar=15){
  i <- seq_len(floor((nvar-1)/2))
  edits <- paste0("x",i,"==", "x", 2*i, "+", "x", 2*i + 1)
  editmatrix(edits)
}

generate_data <- function(E, nerrors=0){
  vars <- getVars(E)
  n <- length(vars)
  
  x <- sapply(vars, function(v) 0)
  x1 <- x
  x1[seq_len(nerrors)] <- -1
  
  x2 <- x
  x2[1+n - seq_len(nerrors)] <- -1  

  x3 <- x
  x3[round((n-nerrors)/2) + seq_len(nerrors)] <- -1  
  as.data.frame(rbind(x1,x2,x3))
}


bench <- function(nvars = 10, nerrors=10, method="bb"){
  
  init <- !file.exists(FILE)
  txt <- file(FILE, "at")
  on.exit(close(txt))
  
  errorloc <- c("begin", "end", "middle")
  
  if (nerrors > nvars) stop("nvars cannot be less than nerrors")
  for (nvar in seq_len(nvars)){
    for (ne in seq(1, min(nerrors, nvar))){
        try({
        E <- generate_balance(nvar)
        data <- generate_data(E, ne)
        cat("\r nvar=", nvar, " ne=", ne, " method=", method)
        le <- localizeErrors(E, data, method=method)
        rpt <- cbind(method=method, nvar=nvar, nerrors=ne, errorloc=errorloc, le$status)
        
        write.table(rpt, file=txt, col.names=init, row.names=FALSE)
        init <- FALSE
        flush(txt)
      })
      gc()
    }
  }
}

## quick testing

if (file.exists(FILE)) file.remove(FILE)

# bench(10,10, method="mip")
bench(100,10, method="mip")
bench(50,10, method="bb")

dat <- read.table(FILE, header=TRUE)
library(ggplot2)
qplot(data=dat, y=elapsed, x=nvar, color=method, facets=nerrors~method, shape=errorloc, geom=c("point", "line")) + geom_jitter()
ggsave("benchmip_balance.png")