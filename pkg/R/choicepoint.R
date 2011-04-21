
#' Choice point: a binary search program
#'
#' \code{choicepoint} creates a binary search program that can be started by calling the searchNext function
#' It walks a binary tree depth first. For all left nodes \code{choiceLeft} is evaluated, for all right nodes 
#' \code{choiceRight} is evaluated. A solution is found if \code{isSolution} evaluates to \code{TRUE}.
#' If \code{isSolution} evaluates to \code{FALSE} it stops at the current node and goes up the next serach node
#' If \code{isSolution} evaluates to NULL it will continue to search deaper.
#'  
#' @export 
#' @example examples/choicepoint.R
#' @param isSolution \code{expression} that should evaluate to \code{TRUE} when a solution is found.
#' @param choiceLeft \code{expression} that will be evaluated for a left node
#' @param choiceRight \code{expression} that will be evaluated for a right node
#' @param list \code{list} with variables that will be added to the search environment
#' @param ... variables that will be added to the search environment
#' 
#' @return choicepoint object
choicepoint <- function(isSolution, choiceLeft, choiceRight, list=NULL, ...){
   
   isSolution <- substitute(isSolution)
   choiceLeft <- substitute(choiceLeft)
   choiceRight <- substitute(choiceRight)
   e <- new.env()
   
   with(e,{
      
      depth <- 0
      maxwidth <- 2
      #TODO add maxdepth as parameter to this function
      maxdepth <- 100
      
      state <- new.env(parent=e)
      state$.width <- 1
      
      l <- c(list, list(...))
      
      if (length(l) > 0){
         list2env(l, state)
      }
            
      searchNext <- function(...){
         state <- e$state
         if (is.null(state)){
           stop("Search completed.")
         }
         
         if (length(l <- list(...))){
            list2env(l, state)
         }
         
         sol <- eval(isSolution, state)
         while (is.null(sol) || !sol){
            if (!is.null(sol)){
               state <- up(state)
               if (is.null(state)){
                  return(NULL)
               }
            }
            width <- state$.width
            state <- down(state)
            if (width == 1){
               eval(choiceLeft, state)
            }
            else {
               eval(choiceRight, state)
            }
            sol <- eval(isSolution, state)
            print(ls.str(state))
         }
         e$state <- up(state)
         
         if (sol) {
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
         cat("up, depth=", depth,"width=", state$.width, "\n")
         state
      }
      
      down <- function(state){
         depth  <<- depth + 1
         if (depth > maxdepth) stop("maxdepth")
         cat("down, depth=", depth,"width=", state$.width, "\n")
         #state$.width <- state$.width + 1
         state <- new.env(parent=state)
         state$.width <- 1
         state
      }


   })
   
   structure(e, class="choicepoint")
}

#' print a choicepoint
#'
#' @method print choicepoint
#' @param x choicepoint object to be printed
print.choicepoint <- function(x){
   print(ls.str(x$state))
}

# cp <- choicepoint( isSolution= { if (y==0) return(TRUE)
                                 # if (x == 0) return(FALSE)
                               # }
                 # , choiceLeft = {x <- x-1;y <- y}
                 # , choiceRight = {y <- y - 1; x <- x}
                 # , x=2
                 # , y=2
                 # )

# cp$searchNext()
# cp$searchNext()
# cp$searchNext()
# cp$searchNext()
