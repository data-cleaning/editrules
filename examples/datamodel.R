
E <- editarray(c(
    "age \%in\% c('under aged','adult')",
    "positionInHouseholda  \%in\% c('marriage partner', 'child', 'other')",
    "maritalStatus \%in\% c('unmarried','married','widowed','divorced')",
    "if (maritalStatus \%in\% c('married','widowed','divorced') ) positionInHousehold != 'child'",
    "if ( age == 'under aged') maritalStatus == 'unmarried'"
    )
)
datamodel(E)
