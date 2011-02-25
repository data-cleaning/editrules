# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)


#Using data.frame to define constraints
E.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
E.df <- read.csv(textConnection(E.csv))			
print(E.df)

E <- editmatrix(E.df)
print(E)
