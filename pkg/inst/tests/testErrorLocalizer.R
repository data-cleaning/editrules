require(testthat)


test_that("errorLocalizer",{
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

})






