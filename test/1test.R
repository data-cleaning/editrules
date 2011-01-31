# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
examples <- sapply(list.files("../examples", full.names=TRUE), source)


# dat <- data.frame( x=1:3
                 # , y=1:3
				 # , z=3:1
				 # , w=3:1)
# cond <- "
# x == y
# z + w == y + x
# x + z == y + 2*w
# "

# mat <- editmatrix(editrules=cond)

# is.editmatrix(mat)
# print(mat)

# editmatrix(editrules=c("-2 * x == y "))