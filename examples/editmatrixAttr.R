
E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
print(E)
                 
# get editrules, useful for storing and maintaining the rules external from your script
editrules(E)
                 
# get constants of inequalities (i.e. c(0, 2))                
getCONSTANT(E)

# get operators of inequalities (i.e. c("==",">"))
getOps(E)
