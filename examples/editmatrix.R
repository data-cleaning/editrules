# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
con <- textConnection(E.csv)
E.df <- read.csv(con)
close(con)
print(E.df)

E <- editmatrix(E.df)
print(E)
