
#' Choice point: a binary search program
#'
#' \code{choicepoint} creates a binary search program that can be started by calling the searchNext function
#' It walks a binary tree depth first. For all left nodes \code{choiceLeft} is evaluated, for all right nodes 
#' \code{choiceRight} is evaluated. A solution is found if \code{isSolution} evaluates to \code{TRUE}.
#' If \code{isSolution} evaluates to NULL it will continue to search deaper.
#' If \code{isSolution} evaluates to \code{FALSE} it stops at the current node and goes up the next search node
#'  
#'
#' @example examples/choicepoint.R
#' @param isSolution \code{expression} that should evaluate to \code{TRUE} when a solution is found.
#' @param choiceLeft \code{expression} that will be evaluated for a left node
#' @param choiceRight \code{expression} that will be evaluated for a right node
#' @param list \code{list} with variables that will be added to the search environment
#' @param ... variables that will be added to the search environment
#' 
#' @return choicepoint object
#' @export
choicepoint <- function(isSolution, choiceLeft, choiceRight, list=NULL, ...){
   
   isSolution <- substitute(isSolution)
   choiceLeft <- substitute(choiceLeft)
   choiceRight <- substitute(choiceRight)
   e <- new.env()
   
   with(e,{
      
      reset <- function(){
         state <- root
         depth <- 0
         
         state$.width <- 1
         state$.path <- NULL
         
         if (length(init) > 0){
            list2env(init, state)
         }
         
         e$state <- state
      }
      
      searchAll <- function(..., VERBOSE=FALSE){
         solutions <- list()
         while (!is.null(sol <- searchNext())){
            solutions[[length(solutions)+1]] <- sol
         }
         return(solutions)
      }
            
      searchNext <- function(..., VERBOSE=FALSE){
         state <- e$state
         if (is.null(state)){
           #search complete
           return(NULL)
         }
         
         if (length(l <- list(...))){
            list2env(l, root)
         }
         
         sol <- eval(isSolution, state)
         while (is.null(sol) || !sol){
            if (!is.null(sol)){
               state <- up(state)
               if (is.null(state)){
                  return(NULL)
               }
               state$.path <- state$.path[1:depth]
            }
            width <- state$.width
            path <- state$.path
            state <- down(state)
            if (width == 1){
               state$.path <- c(path, "left")
               eval(choiceLeft, state)
            }
            else {
               state$.path <- c(path, "right")
               eval(choiceRight, state)
            }
            sol <- eval(isSolution, state)
            
            if (VERBOSE){
               cat("path:",paste(state$.path, collapse="->", sep=""),", solution : ", sol,"\n")
               print(ls.str(state))
            }
         }
         e$state <- up(state)
         
         if (sol) {
            currentSolution <<- state
            return(as.list(state))
         }
      }
      
      up <- function(state){
         depth <<- depth - 1
         if (depth == -1){
            return(NULL)
         }
         state <- parent.env(state)
         state$.width <- state$.width + 1
         if (state$.width > maxwidth){
            return(up(state))
         }
         #cat("up, depth=", depth,"width=", state$.width, "\n")
         state
      }
      
      down <- function(state){
         depth  <<- depth + 1
         if (depth > maxdepth) stop("maxdepth")
         #cat("down, depth=", depth,"width=", state$.width, "\n")
         #state$.width <- state$.width + 1
         state <- new.env(parent=state)
         state$.width <- 1
         state
      }
      depth <- 0
      maxwidth <- 2
      #TODO add maxdepth as parameter to this function
      maxdepth <- 100
      currentSolution <- NULL
      root <- new.env(parent=e)
      init <- c(list, list(...))
      reset()
   })
   
   structure(e, class="choicepoint")
}


#' print a choicepoint
#'
#' @export
#' @method print choicepoint
#' @param x choicepoint object to be printed
#' @param ... other parameters passed to print method
#' @param VERBOSE should all variables be printed?
print.choicepoint <- function(x, ..., VERBOSE=FALSE){
   print(ls.str(x$state, all.names=VERBOSE))
}

#' iterate over all solutions of a \code{\link{choicepoint}}
#'
#' iterate over all solutions of a \code{\link{choicepoint}}
#' This method is identical to calling \code{$searchNext} on a \code{choicepoint}
#' 
#' @export
#' @method iter choicepoint
#' @param x \code{\link{choicepoint}} object
#' @return choicepoint iterator
iter.choicepoint <- function(x){
   # TODO add stop iteration
   
   x$nextElem <- function(){ 
      sol <- x$searchNext()
      if (is.null(sol)){
         stop("StopIteration", call.=FALSE)
      }
      sol
   }
   class(x) <- c("abstractiter","iter", "choicepoint")
   x
}

