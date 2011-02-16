# Using character vector to define contraints
E <- editmatrix(editrules=c("x+3*y==2*z", "x==z"))
print(E)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
valid <- checkRows(E, dat)
invalid <- dat[!valid,] 
listErrors(E,invalid)
