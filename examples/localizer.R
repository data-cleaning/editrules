
source("localize.R")


E <- editmatrix(c("p + c == t"))

dat <- data.frame(
    p = 1,
    c = 1,
    t = 3)

cc <- localizeErrors(E,x=c(p=1,c=1,t=3),rep(1,3))





