
cp <- choicepoint( isSolution= { 
                                 if (y==0) return(TRUE)
                                 if (x == 0) return(FALSE)
                               }
                 , choiceLeft = { x <- x - 1; y <- y}
                 , choiceRight = { y <- y - 1; x <- x}
                 , x=2
                 , y=1
                 )

cp$searchNext(VERBOSE=TRUE)
cp$searchNext(VERBOSE=TRUE)

# next search will return NULL because there is no more solution
cp$searchNext()


cp$reset()

# choicepoint also works with iterators
if (require(iterators)){
   it <- iter(cp)
   nextElem(it)
   nextElem(it)
}


