# derive implicit numerical edits by fourier-motzkin elimination.
# alternative implementation
eliminate <- function(E, var, assumeNormalized=TRUE){
    if (!assumeNormalized) E <- normalize(E)
    vars <- getVars(E)
    
    if (!var %in% vars){ # nothing to eliminate...
        return(E)
    }
    
    m <- getMatrix(E)
    ops <- getOps(E)
    C <- getC(E)
    
    coefs <- m[,var]
    I <- coefs != 0
    
    # coefs <- coefs[I]
    # ops <- getOps(E)[I]
    m <- cbind(m[,,drop=FALSE], C)
    
    eq <- I & (ops == "==")
    
    upper <- which(!eq & coefs > 0)
    lower <- which(!eq & coefs < 0)
    
    coefs[!eq] <- abs(coefs[!eq])
    
    eq <- which(eq);
    #normalize matrix, every row has coefficient 1 or -1 for var
    m[I,] <- m[I,] / coefs[I]
    ml <- list()
    ol <- character(0)
    equpper <- c(eq, upper)
    for (lb in lower){
       mlb <- m[lb,]
       ml <- append(ml, lapply( equpper
                              , function(b){
                                    mlb + m[b,]
                                }
                              )
                   )
       ol <- c(ol, ifelse(ops[equpper] != "<", ops[lb], ops[equpper]))
    }
    
    for (eb in eq){
       equpper <- equpper[-1]
       meb <- m[eb,]
       ml <- append(ml, lapply( equpper
                              , function(b){
                                   m[b,] - meb
                                }
                              )
                   )
       ol <- c(ol, ops[equpper])                   
    }
    #print(ol)
    
    names(ml) <- paste("d", seq_along(ml), sep="")
    
    ml <- append(ml,lapply(which(!I),function(i){m[i,]}))
    ol <- c(ol,ops[!I])
    
    m <- do.call(rbind,ml)
    as.editmatrix(m[,-ncol(m),drop=FALSE], m[,ncol(m)], ol)
}

