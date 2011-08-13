# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z"
                  , "x==z"
                  )
                )
                
dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)
				 
violatedEdits(E,dat)
