# Using character vector to define contraints
em <- editmatrix(c( "x+3*y==2*z"
                  , "x==z"
                  )
                )
print(em)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
checkRows(em, dat)
