# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
examples <- sapply(list.files("../examples", full.names=TRUE), source)

cond <- "
x == y
z + w == y + x
x + z == y + 2*w
"

mat <- editmatrix(editrules=cond)

er <- editrules(mat)

er$rule <- er$edit
er$edit <- NULL

em <- editmatrix(er)
print(em)