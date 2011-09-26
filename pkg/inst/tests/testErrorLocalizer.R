require(testthat)
context("Error Localization")

test_that("errorLocalizer for numerical data",{
    bt <- errorLocalizer(
            E = editmatrix("x + y == z"),
            x = c(x=1,y=1,z=2))
    expect_true( all(bt$searchNext()$adapt==c(FALSE,FALSE,FALSE)) )
    expect_true( is.null(bt$searchNext()))
    expect_true(1 == errorLocalizer(editmatrix("x+y==z"),c(x=1,y=1,z=3))$searchNext()$w)
    expect_true(1 == errorLocalizer(editmatrix("x+y==z"),c(x=1,y=1,z=3,u=5))$searchNext()$w)
    expect_true(is.null(errorLocalizer(editmatrix("x+y==z"),c(x=1,y=1,z=3))$searchNext(maxduration=-1)) )
    expect_true(is.null(errorLocalizer(editmatrix("x+y==z"),c(x=1,y=1,z=3),maxadapt=0)$searchNext()) )
    expect_true(is.null(errorLocalizer(editmatrix("x+y==z"),c(x=1,y=1,z=3),maxweight=0)$searchNext()) )
    expect_that(errorLocalizer(editmatrix("x+y==z"),c(x=1,y=NA,z=3),weight=c(1,NA,1))$searchNext(),throws_error())
})



#d <- "../../../pkg/R"
#for ( b in file.path(d,dir(d)) ) dmp <- source(b,echo=FALSE)

test_that("errorLocalizer for categorical data",{
    E <- editarray(c(
        "positionInHouseHold %in% c('marriage partner','child','other')",
        "age %in% c('child','adult')",
        "maritalStatus %in% c('unmarried','married','widowed')",
        "if (age == 'child') maritalStatus == 'unmarried'",
        "if (maritalStatus == 'unmarried') positionInHouseHold != 'marriage partner'"
        ))

r <- c(age='child',maritalStatus='married',positionInHouseHold='child')
e <- errorLocalizer(E,r)
e$searchNext()

})



