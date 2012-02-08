#' Decompose matrix or edits in independent blocks
#'
#' For matrices, a list of matrices is returned which direct sum to the original matrix.
#' For objects of class \code{\link{editmatrix}}, \code{\link{editarray}} or \code{\link{editset}}, 
#' lists of corresponding objects not sharing any variable are returned.
#'
#' @param M Object to be decomposed into 
#' @param ... Arguments to be passed to or from other functions/
#' @return list of independent subobjects of \code{M}.
#' @example ../examples/blocks.R
#' @export
blocks <- function(M,...){
    blocks <- blockIndex(contains(M))
    lapply( blocks, 
        function(b){
            reduce(M[b,,drop=FALSE])
        }
    )
}


#' Determine list of indices indicating blocks in logical matrix
#' 
#' 
#' @param D matrix of type \code{logical}
#' @return \code{list} of row indices in \code{D} indicating independent blocks.
#'      Empty rows (i.e. every column \code{FALSE}) are ignored.
#' @example ../examples/blocks.R
#' 
#' @export
blockIndex <- function(D){

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
    orignames <- row.names(D)
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
    lapply(blocks,function(b) {names(b)<-orignames[b]; b})
}









