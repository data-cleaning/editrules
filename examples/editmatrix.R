# Using character vector to define contraints
em <- editmatrix(editrules=c("x+3*y==2*z", "x==z"))
print(em)

#Using editsinfo to define constraints
ei <- editsinfo(em)
ei$name <- c("Rule A", "Rule B")
ei$description <- c("This is rule a", "This is rule b")

em <- editmatrix(ei)
print(em)
