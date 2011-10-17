
# an editmatrix and some data:
E <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0"))

dat <- data.frame(
    x = c(1,-1,1),
    y = c(-1,1,1),
    z = c(2,0,2))

# localize all errors in the data
err <- localizeErrors(E,dat)

## Not run

# Demonstration of verbose processing
# construct 2-block editmatrix
F <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0",
    "w > 10"))
# User 'dat' as above, generate some extra records
dd <- dat
for ( i in 1:5 ) dd <- cbind(dd,dd)

# localize errors verbosely
err <- localizeErrors(E,dd,verbose=TRUE)

## End(Not run)


# what has to be adapted:
err$adapt
# weight, number of equivalent solutions, timings,
err$status

# an example with categorical variables
E <- editarray(c(
    "age \%in\% c('under aged','adult')",
    "maritalStatus \%in\% c('unmarried','married','widowed','divorced')",
    "positionInHousehold \%in\% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus \%in\% c('married','widowed','divorced')) !positionInHousehold \%in\% c('marriage partner','child')"
    )
)
E

#
dat <- data.frame(
    age = c('under aged','adult','adult' ),
    maritalStatus=c('married','unmarried','widowed' ), 
    positionInHousehold=c('child','other','marriage partner')
)
dat
localizeErrors(E,dat)
# the last record of dat has 2 degenerate solutions. Running  the last command a few times
# demonstrates that one of those solutions is chosen at random.

# Increasing the weight of  'positionInHousehold' for example, makes the best solution
# unique again
localizeErrors(E,dat,weight=c(1,1,2))

