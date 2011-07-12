#' @nord
findBlocks <- function(M){
    warning("findBlocks is deprecated, use 'blocks' in stead")
    blocks(M)
}

#' Break a matrix into blocks
#'
#' @example examples/blocks.R
#' @aliases findBlocks
#' @param M \code{matrix} or \code{editmatrix} that will be broken in blocks
#' @return list with seperate matrix blocks
#' @export
blocks <- function(M){
   
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
       x <- colSums(B[b,,drop=FALSE]) > 0 #this is another way of "or"ring all found rows
     }
     b
   }
   
   if (is.editmatrix(M)){
      m <- getA(M)
   }
   else {
      m <- as.matrix(M)
   }
   
   D <- m != 0
   
   row.names(D) <- 1:nrow(D)
   
   #remove empty rows
   b <- rowSums(D) == 0
   D <- D[!b,,drop=FALSE]
   
   # create a list which will contain the blocks
   blocks <- list()
   L <- 1
   
   # detect and remove blocks until no blocks are left
   while (nrow(D) > 0){
      
      # find block
      b <- block(D)
      
      # store the original row numbers of the detected block
      blocks[[L]] <- as.integer(row.names(D)[b])
      L <- L + 1
      
      # remove the detected block
      D <- D[!b,,drop=FALSE]
   }
   
   #return decomposed original matrix 
   lapply( blocks
         , function(b){
              removeEmpty(M[b,,drop=FALSE])
           }
         )
}
