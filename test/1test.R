# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
#examples <- sapply(list.files("../examples", full.names=TRUE), source)

x <- editmatrix(c("x ==2", "x - 2 < y + 1"))
x

as.character(x)
as.expression(x)
