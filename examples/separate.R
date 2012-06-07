

E <- editset(expression(
    x + y == z,
    2*u  + 0.5*v == 3*w,
    w >= 0,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A %in% letters[1:4],
    B %in% letters[1:4],
    C %in% c(TRUE,FALSE),
    D %in% letters[5:8],
    if ( A %in% c('a','b') ) y > 0,
    if ( A == 'c' ) B %in% letters[1:3],
    if ( !C == TRUE) D %in% c('e','f')
))

(L <- separate(E))

sapply(L,class)






