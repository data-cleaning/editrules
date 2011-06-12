
#' Choice point: a binary search program
#'
#' \code{choicepoint} creates a binary search program that can be started by calling the \code{$searchNext} function
#' It walks a binary tree depth first. For all left nodes \code{choiceLeft} is evaluated, for all right nodes 
#' \code{choiceRight} is evaluated. A solution is found if \code{isSolution} evaluates to \code{TRUE}.
#' If \code{isSolution} evaluates to NULL it will continue to search deaper.
#' If \code{isSolution} evaluates to \code{FALSE} it stops at the current node and goes up the next search node
#'
#' \subsection{Methods}{
#'   \describe{
#'      \item{\code{$searchNext(..., VERBOSE=FALSE)}}{Search next solution, can
#'           be called repeatedly until there is no solution left. Named variables will be
#'           added to the search environment, this feature can be used to direct the search
#'           in subsequent calls to \code{searchNext}. VERBOSE=TRUE will print all
#'           intermediate search steps and results. It can be used to debug the expressions
#'           in the choicepoint}
#'      \item{\code{$searchAll(..., VERBOSE=FALSE)}}{Return all solutions as a list}
#'      \item{\code{$reset()}}{Resets the \code{choicepoint} to its initial state.}
#'   }
#' }
#' @example examples/choicepoint.R
#'
#' @param isSolution \code{expression} that should evaluate to \code{TRUE} when a solution is found.
#' @param choiceLeft \code{expression} that will be evaluated for a left node
#' @param choiceRight \code{expression} that will be evaluated for a right node
#' @param list \code{list} with variables that will be added to the search environment
#' @param ... named variables that will be added to the search environment
#' 
#' @return choicepoint object, see Methods for a description of the methods
#' @export
#'
choicepoint <- function(isSolution, choiceLeft, choiceRight, list=NULL, ...){
   
   isSolution <- substitute(isSolution)
   choiceLeft <- substitute(choiceLeft)
   choiceRight <- substitute(choiceRight)
   e <- new.env()
   
   with(e,{
      
      reset <- function(){
         e$state <- root
         e$depth <- 0
         
         state$.width <- 1
         state$.path <- NULL
         
         if (length(init) > 0){
            list2env(init, state)
         }
         
      }
      
      searchAll <- function(..., VERBOSE=FALSE){
         solutions <- list()
         while (!is.null(sol <- searchNext(..., VERBOSE=VERBOSE))){
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
               cat("***********************************************************************\n")
               cat("path:",paste(state$.path, collapse="->", sep=""),", solution : ", sol,"\n")
               print(ls.str(envir=state))
            }
         }
         e$state <- up(state)
         
         if (sol) {
            currentSolution <<- state
            return(as.list(state, all.names=VERBOSE))
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
#' This method is identical to calling \code{$searchNext} on a \code{choicepoint}. Please note that iterating
#' a choicepoint changes the state of a choicepoint.
#' 
#' @export
#' @method iter choicepoint
#' @param x \code{\link{choicepoint}} object
#' @param ... extra parameters that will given to the \code{searchNext()} function
#' @return choicepoint iterator
#' @seealso \code{iter} from the package iterators
iter.choicepoint <- function(x, ...){
   # TODO add stop iteration
   
   x$nextElem <- function(){ 
      sol <- x$searchNext(...)
      if (is.null(sol)){
         stop("StopIteration", call.=FALSE)
      }
      sol
   }
   class(x) <- c("abstractiter","iter", "choicepoint")
   x
}

