
# edits can be read from a vector of expressions 
E <- editset(expression(
    if ( x > 0 ) y > 0,
    x + y == z,
    A %in% letters[1:2],
    B %in% letters[2:3],
    if ( A == 'a') B == 'b',
    if ( A == 'b') x >= 0,
    u + v == w,
    if ( u == 0 ) w >= 0
))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')


# see also editfile
E <- editfile(system.file('script/edits/mixedits.R',package='editrules'))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')



