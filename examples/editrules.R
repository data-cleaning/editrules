# load a data.frame with the edit rules
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)

print(E.df)
# Create the editmatrix from the editrules
E <- editmatrix(E.df)
print(E)

#data that should conform to the constraints
dat <- data.frame( w = c(0,0,0)
                 , x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)

# valid rows?
checkRows(E, dat)

#ok, lets show the errors
violatedEdits(E,dat)

#or, lets list the contraints that were violated
listViolatedEdits(E,dat)
