editrules
=========

R package for parsing edit rules
The editrules package aims to provide an environment to conveniently define, read and check recordwise data constraints including
Linear (in)equality constraints for numerical data
Constraints on value combinations of categorical data
Conditional constraints on numerical and/or mixed data
In literature these constraints, or restrictions are refered to as ``edits''. editrules can perform common rule set manipulations like variable elimination and value substitution, and offers error localization functionality based on the (generalized) paradigm of Fellegi and Holt. Under this paradigm, one determines the smallest (weighted) number of variables to adapt such that no (additional or derived) rules are violated. The paradigm is based on the assumption that errors are distributed randomly over the variables and there is no detectable cause of error. It also decouples the detection of corrupt variables from their correction. For some types of error, such as sign flips, typing errors or rounding errors, this assumption does not hold. These errors can be detected and are closely related to their resolution. The reader is referred to the deducorrect? package for treating such errors.

To install the latest version in R:
```
install.packages("editrules")
```

Typical usage is:

```
# load a data.frame with the edit rules
E.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
E.df <- read.csv(textConnection(E.csv))                 

print(E.df)
# Create the editmatrix from the editrules
E <- editmatrix(E.df)
print(E)

#data that should conform to the constraints
dat <- data.frame( w = c(0,0,0)
                 , x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)

# valid rows?
checkRows(E, dat)

#ok, lets show the errors
violatedEdits(E,dat)

#or, lets list the contraints that were violated
listViolatedEdits(E,dat)
```
