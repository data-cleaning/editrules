
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

# the \%in\% statement may be used at will
editarray(c(
    "gender \%in\% c('male','female')",
    "pregnant \%in\% c(TRUE, FALSE)",
    "positionInHousehold \%in\% c('marriage partner', 'child', 'other')",
    "maritalStatus \%in\% c('unmarried','married','widowed','divorced')",
    "if( gender == 'male' ) !pregnant",
    "if( maritalStatus \%in\% c('married','widowed','divorced')) !positionInHousehold \%in\% c('marriage partner','child')"
    )
)





