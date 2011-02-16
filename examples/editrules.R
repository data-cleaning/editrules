# load a data.frame with the edit rules
edtrules.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
edtrules <- read.csv(textConnection(edtrules.csv))			

# Create the editmatrix from the editrules
E <- editmatrix(edtrules)
print(E)

#data that should conform to the constraints
dat <- data.frame( w = c(0,0,0)
                 , x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
checkRows(E, dat)

#ok, lets show the errors
errorMatrix(E,dat)

#or, lets list the contraints that were violated
listErrors(E,dat)
