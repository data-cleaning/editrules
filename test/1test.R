# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
examples <- sapply(list.files("../examples", full.names=TRUE), source)
