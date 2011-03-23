#' Break a matrix in blocks
#'
#' @example examples/findBlocks.R
#' @nord
#'
#' @param M \code{matrix} or \code{editmatrix} that will be broken in blocks
#' @return list with seperate matrix blocks
findBlocks <- function(M){
   
   block <- function(B){
     x1 <- FALSE
     x <- B[1,]
     while (sum(x1 != x)){
       x1 <- x
       b <- sapply( 1:nrow(B)
                  , function(i){
                    any(B[i,] & x)
                   }
                  )
       x <- colSums(B[b,,drop=FALSE]) > 0 #this another way of "or"ring all found rows
     }
     b
   }
   m <- as.matrix(M)
   
   D <- m != 0
   row.names(D) <- 1:nrow(D)
   
   #remove empty rows
   b <- rowSums(D) == 0
   D <- D[!b,,drop=FALSE]
   blocks <- list()
   L <- 1
   while (nrow(D) > 0){
      b <- block(D)
      blocks[[L]] <- as.integer(row.names(D)[b])
      L <- L + 1
      D <- D[!b,,drop=FALSE]
    }
   lapply( blocks
         , function(b){
              removeEmpty(M[b,,drop=FALSE])
           }
         )
}