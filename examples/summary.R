
#B <- dir('../pkg/R',full.names=TRUE)
#for ( b in B ) dmp <- source(b)

#load('../pkg/data/edits.RData')

data(edits)
E <- editmatrix(edits)
summary(E)



