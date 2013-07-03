
bt <- backtracker( isSolution= { 
                                 if (y == 0) return(TRUE)
                                 if (x == 0) return(FALSE)
                               }
                 , choiceLeft = { x <- x - 1; y <- y}
                 , choiceRight = { y <- y - 1; x <- x}
                 # starting values for x and y
                 , x=2
                 , y=1
                 )

bt$searchNext(VERBOSE=TRUE)
bt$searchNext(VERBOSE=TRUE)

# next search will return NULL because there is no more solution
bt$searchNext()

bt$reset()




