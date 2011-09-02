#### examples with numerical edits
# example with a single editrule
# p = profit, c = cost, t = turnover
E <- editmatrix(c("p + c == t"))
cp <- errorLocalizer(E, x=c(p=755, c=125, t=200))
# x obviously violates E. With all weights equal, changing any variable will do.
# first solution:
cp$searchNext()
# second solution:
cp$searchNext()
# third solution:
cp$searchNext()
# there are no more solution since changing more variables would increase the weight,
# so the result of the next statement is NULL:
cp$searchNext()

# Increasing the reliability weight of turnover, yields 2 solutions:
cp <- errorLocalizer(E, x=c(p=755, c=125, t=200), weight=c(1,1,2))
# first solution:
cp$searchNext()
# second solution:
cp$searchNext()
# no more solutions available:
cp$searchNext()


# A case with two restrictions. The second restriction demands that
# c/t >= 0.6 (cost should be more than 60% of turnover)
E <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0"))
cp <- errorLocalizer(E,x=c(p=755,c=125,t=200))
# Now, there's only one solution, but we need two runs to find it (the 1st one has higher weight)
cp$searchNext()
cp$searchNext()

# With the searchBest() function, the lowest weifght solution is found at once:
errorLocalizer(E,x=c(p=755,c=125,t=200))$searchBest()


# An example with missing data.
E <- editmatrix(c(
    "p + c1 + c2 == t",
    "c1 - 0.3*t >= 0",
    "p > 0",
    "c1 > 0",
    "c2 > 0",
    "t > 0"))
cp <- errorLocalizer(E,x=c(p=755, c1=50, c2=NA,t=200))
# (Note that e2 is violated.)
# There are two solutions. Both demand that c2 is adapted:
cp$searchNext()
cp$searchNext()

##### Examples with categorical edits
# 
# 3 variables, recording age class, position in household, and marital status:
# We define the datamodel and the rules
E <- editarray(c(
    "age \%in\% c('under aged','adult')",
    "maritalStatus \%in\% c('unmarried','married','widowed','divorced')",
    "positionInHousehold \%in\% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus \%in\% c('married','widowed','divorced')) !positionInHousehold \%in\% c('marriage partner','child')"
    )
)
E

# Let's define a record with an obvious error:
r <- c(age = 'under aged', maritalStatus='married', positionInHousehold='child')
# The age class and position in household are consistent, while the marital status conflicts. 
# Therefore, changing only the marital status (in stead of both age class and postition in household)
# seems reasonable. 
el <- errorLocalizer(E,r)
el$searchNext()










