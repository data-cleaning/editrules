

E <- editmatrix(expression(
    x + y == z,
    2*y < 10,
    3*x + 1.5*u < 7
    z >= 0))

# single value
substValue(E,'z',10)
# multiple values
substValue(E,c('x','y'),c(1,3))
# remove substituted variable from edits
substValue(E,'z',10,reduce=TRUE)
# do not remove redundant row:
substValue(E,'z',10,removeredundant=FALSE)


# example with an editset
E <- editset(expression(
    x  + y == z,
    x >= 0,
    y >= 0,
    A %in% c('a1','a2'),
    B %in% c('b1','b2'),
    if ( x > 0 ) y > 0,
    if ( y > 0 ) x > 0,
    if ( A == 'a' ) B == 'b',
    if ( A == 'b' ) y > 3
    )
)

# substitute pure numerical variable
substValue(E,'z',10)
# substitute pure categorical variable
substValue(E,'A','a1')
# substitute variable appearing in logical constraints
substValue(E,'x',3)



