# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z"
                  , "x==z"
                  )
                )
                
dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)

ve <- violatedEdits(E,dat)

print(ve)
summary(ve, E)
plot(ve)

# An example with categorical data:

E <- editarray(expression(
    gender %in% c('male','female'),
    pregnant %in% c(TRUE, FALSE),
    if( gender == 'male' ) !pregnant
    )
)
print(E)

dat <- data.frame(
    gender=c('male','male','female','cylon'), 
    pregnant=c(TRUE,FALSE,TRUE,TRUE)
)

print(dat)
# Standard, the datamodel is checked as well,
violatedEdits(E,dat)

# but we may turn this of
violatedEdits(E,dat,datamodel=FALSE)


