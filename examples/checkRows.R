# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z",
                  "x==z"))
print(E)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows? editmatrix method
checkRows(E, dat)


# example using the character method.
data(women)
e <- c("height<=71", "weight/height>=2.1")
valid <- checkRows(e,women)
women[valid,]

# same example, using data.frame method
ef <- data.frame(
    name=c("rule 1", "rule 2"), 
    edit=e, 
    description=c("descr1","descr2"))
checkRows(ef,women)
