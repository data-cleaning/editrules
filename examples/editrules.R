# load a data.frame with the edit rules
edtrules.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
edtrules <- read.csv(textConnection(edtrules.csv))			

# Create the editmatrix from the editrules
em <- editmatrix(edtrules)
print(em)

#data that should conform to the constraints
dat <- data.frame( w = c(0,0,0)
                 , x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
checkRows(em, dat)

#ok, lets show the errors
errorMatrix(em,dat)

#or, lets list the contraints that were violated
listErrors(em,dat)
