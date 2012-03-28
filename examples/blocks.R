# three seperate blocks
E <- editmatrix( expression( 
  x1 + x2 == x3,
  x3 + x4 == x5,
  x5 + x6 == x7,
  y1 + y2 == y3,
  z1 + z2 == z3
))
blocks(E)

# four seperate blocks
E <- editmatrix(expression( 
  x1 + x2 == x3,
  x3 + x4 == x5,
  x8 + x6 == x7,
  y1 + y2 == y3,
  z1 + z2 == z3
))
blocks(E)

# two categorical blocks
E <- editarray(expression(
 x %in% c('a','b','c'),
 y %in% c('d','e'),
 z %in% c('f','g'),
 u %in% c('w','t'),
 if ( x == 'a') y != 'd',
 if ( z == 'f') u != 'w'
))
blocks(E)


