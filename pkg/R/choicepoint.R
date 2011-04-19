

choicepoint <- function(isSolution, choiceLeft, choiceRight, list=NULL, ...){
   
   isSolution <- substitute(isSolution)
   choiceLeft <- substitute(choiceLeft)
   choiceRight <- substitute(choiceRight)
   e <- new.env()
   
   with(e,{
      
      depth <- 0
      maxwidth <- 2
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
