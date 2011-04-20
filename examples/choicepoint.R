
cp <- choicepoint( isSolution= { 
                                 if (y==0) return(TRUE)
                                 if (x == 0) return(FALSE)
                               }
                 , choiceLeft = { x <- x - 1; y <- y}
                 , choiceRight = { y <- y - 1; x <- x}
                 , x=1
                 , y=1
                 )

cp$searchNext()

# next search fails because there is no more solution
#cp$searchNext()
