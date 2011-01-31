# Using character vector to define contraints
em <- editmatrix(editrules=c("x+3*y==2*z", "x==z"))
print(em)

#Using editsinfo to define constraints
edtinf.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'

edtinf <- read.csv(textConnection(edtinf.csv))			
em <- editmatrix(edtinf)
print(em)

