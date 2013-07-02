
# The following is an example by Williams (1986). Eliminating all variables
# except z maximizes -4x1 + 5x2 +3x3:
P <- editmatrix(c(
     "4*x1 - 5*x2 - 3*x3 + z <= 0",
     "-x1 + x2 -x3 <= 2",
     "x1 + x2 + 2*x3 <= 3",
     "-x1 <= 0",
     "-x2 <= 0",
     "-x3 <= 0"))
# eliminate 1st variable
(P1 <- eliminate(P, "x1", fancynames=TRUE))
# eliminate 2nd variable. Note that redundant rows have been eliminated
(P2 <- eliminate(P1, "x2", fancynames=TRUE))
# finally, the answer:
(P3 <- eliminate(P2, "x3", fancynames=TRUE))

# check which original edits were used in deriving the new ones
getH(P3)

# check how many variables were eliminated
geth(P3)


# An  example with an equality and two inequalities
# The only thing to do is solving for x in e1 and substitute in e3.
(E <- editmatrix(c(
    "2*x + y == 1",
    "y > 0",
    "x > 0"),normalize=TRUE))
eliminate(E,"x", fancynames=TRUE)


# This example has two equalities, and it's solution 
# is the origin (x,y)=(0,0)
(E <- editmatrix(c(
    "y <= 1 - x",
    "y >= -1 + x",
    "x == y",
    "y ==-2*x" ),normalize=TRUE))
eliminate(E,"x", fancynames=TRUE)

# this example has no solution, the equalities demand (x,y) = (0,2)
# while the inequalities demand y <= 1
(E <- editmatrix(c(
    "y <= 1 - x",
    "y >= -1 + x",
    "y == 2 - x",
    "y == -2 + x" ),normalize=TRUE))
# this happens to result in an obviously unfeasable system:
isObviouslyInfeasible(eliminate(E,"x"))


# for categorical data, elimination amounts to logical derivartions. For
# example
E <- editarray(expression(
    age %in% c('under aged','adult'),
    positionInHousehold %in% c('marriage partner', 'child', 'other'),
    maritalStatus %in% c('unmarried','married','widowed','divorced'),
    if (maritalStatus %in% c('married','widowed','divorced') ) 
        positionInHousehold != 'child',
    if (maritalStatus == 'unmarried') 
        positionInHousehold != 'marriage partner' ,
    if ( age == 'under aged') maritalStatus == 'unmarried'
    )
)
E

# by eliminating 'maritalStatus' we can deduce that under aged persones cannot
# be partner in marriage.
eliminate(E,"maritalStatus")

E <- editarray(expression(
    age %in% c('under aged','adult'),
    positionInHousehold %in% c('marriage partner', 'child', 'other'),
    maritalStatus %in% c('unmarried','married','widowed','divorced'),
    if (maritalStatus %in% c('married','widowed','divorced') ) 
        positionInHousehold != 'child',
    if (maritalStatus == 'unmarried') 
        positionInHousehold != 'marriage partner' ,
    if ( age == 'under aged') 
        maritalStatus == 'unmarried'
    )
)
E

# by eliminating 'maritalStatus' we can deduce that under aged persones cannot
# be partner in marriage.
eliminate(E,"maritalStatus")





