

E <- editset(expression(
    x + y == z,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A %in% letters[1:4],
    B %in% letters[1:4],
    if (A %in% c('a','b')) y > 0,
    if (A == 'c' ) B %in% letters[1:3]
))

disjunct(E)




