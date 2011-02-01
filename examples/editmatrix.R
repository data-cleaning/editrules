# Using character vector to define contraints
em <- editmatrix(c("x+3*y==2*z", "x==z"))
print(em)

#Using data.frame to define constraints
editRules.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'

editRules <- read.csv(textConnection(editRules.csv))			
em <- editmatrix(editRules)
print(em)
