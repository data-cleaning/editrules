
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

summary(err)

# what has to be adapted:
err$adapt
# weight, number of equivalent solutions, timings,
err$status


## Not run

# Demonstration of verbose processing
# construct 2-block editmatrix
F <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0",
    "w > 10"))
# Using 'dat' as defined above, generate some extra records
dd <- dat
for ( i in 1:5 ) dd <- rbind(dd,dd)
dd$w <- sample(12,nrow(dd),replace=TRUE)

# localize errors verbosely
(err <- localizeErrors(F,dd,verbose=TRUE))

# printing is cut off, use summary for an overview
summary(err)

# or plot (not very informative in this artificial example)
plot(err)

## End(Not run)


# Example with different weights for each record
E <- editmatrix('x + y == z')
dat <- data.frame(
    x = c(1,1),
    y = c(1,1),
    z = c(1,1))

# At equal weights, both records have three solutions (degeneracy): adapt x, y or z:
localizeErrors(E,dat)$status

# Set different weights per record (lower weight means lower reliability):
w <- matrix(c(
    1,2,2,
    2,2,1),nrow=2,byrow=TRUE)

localizeErrors(E,dat,weight=w)


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




