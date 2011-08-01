
# Here is the prototypical categorical edit: men cannot be pregnant.
editarray(c(
    "gender \%in\% c('male','female')",
    "pregnant \%in\% c('yes','no')",
    "if( gender == 'male' ) pregnant == 'no'"
    )
)

# A yes/no variable may also be modeled as a logical:
editarray(c(
    "gender \%in\% c('male','female')",
    "pregnant \%in\% c(TRUE, FALSE)",
    "if( gender == 'male' ) pregnant == FALSE"
    )
)

# or, shorter: 
editarray(c(
    "gender \%in\% c('male','female')",
    "pregnant \%in\% c(TRUE, FALSE)",
    "if( gender == 'male' ) !pregnant"
    )
)



