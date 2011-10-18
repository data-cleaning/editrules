# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# an editmatrix also has a summary method:
summary(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)
print(E.df)

E <- editmatrix(E.df)
print(E)
