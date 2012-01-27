# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
examples <- sapply(list.files("../examples", full.names=TRUE), source)

x <- editmatrix(c( "x ==2"
                 , "x - 2 > y + 1"
                 , "x>1"
                 , "y>-2"
                 ,"-2>x"
                 ,"-10 < -2"
                 )
                )


x
# dat <- data.frame(x=1:2, y=3:2)

# editrules(x)
# checkRows(x,dat)
# checkRows(as.character(x),dat)
# checkRows(editrules(x),dat)
